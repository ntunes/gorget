/// Statement codegen: convert Gorget statements to C statements.
use crate::parser::ast::{BinaryOp, Block, Expr, Ownership, Pattern, Stmt, Type};
use crate::parser::visitor::{self, ExprVisitor};
use crate::semantic::ids::ScopeId;
use crate::semantic::scope::{DefKind, ScopeTable};
use crate::semantic::types::TypeTable;
use crate::span::{Span, Spanned};

use super::c_emitter::CEmitter;
use super::c_mangle;
use super::c_string_escape;
use super::c_types;
use super::{CodegenContext, DropAction, DropEntry, DropScopeKind};

impl CodegenContext<'_> {
    /// Generate C code for a block of statements.
    pub fn gen_block(&mut self, block: &Block, emitter: &mut CEmitter) {
        for stmt in &block.stmts {
            self.gen_stmt(&stmt.node, stmt.span, emitter);
        }
    }

    // ─── Drop Scope Helpers ─────────────────────────────────

    /// Push a new drop scope onto the stack.
    pub fn push_drop_scope(&mut self, kind: DropScopeKind) {
        self.drop_scopes.push((kind, Vec::new()));
    }

    /// Pop the top drop scope and emit cleanup for its entries.
    pub fn pop_drop_scope(&mut self, emitter: &mut CEmitter) {
        if let Some((_kind, entries)) = self.drop_scopes.pop() {
            for entry in entries.iter().rev() {
                Self::emit_drop_entry(entry, 0, emitter);
            }
        }
    }

    /// Emit cleanup for all scopes from innermost up to and including the
    /// first scope matching `kind`. Does NOT pop any scopes.
    pub fn emit_cleanup_to(&mut self, kind: DropScopeKind, emitter: &mut CEmitter) {
        for (scope_kind, entries) in self.drop_scopes.iter().rev() {
            for entry in entries.iter().rev() {
                Self::emit_drop_entry(entry, 0, emitter);
            }
            if *scope_kind == kind {
                break;
            }
        }
    }

    /// Register a droppable variable in the current (topmost) scope.
    pub fn register_droppable(&mut self, var_name: &str, action: DropAction) {
        if let Some((_kind, entries)) = self.drop_scopes.last_mut() {
            entries.push(DropEntry {
                var_name: var_name.to_string(),
                action,
            });
        }
    }

    /// Check if any drop scope currently has droppable entries.
    fn has_droppable_entries(&self) -> bool {
        self.drop_scopes
            .iter()
            .any(|(_, entries)| !entries.is_empty())
    }

    /// Emit the C cleanup code for a single drop entry.
    /// `depth` is used for unique loop variable names in nested collection drops.
    pub(super) fn emit_drop_entry(entry: &DropEntry, depth: usize, emitter: &mut CEmitter) {
        let idx = format!("__di{depth}");
        let elem = format!("__de{depth}");
        match &entry.action {
            DropAction::BoxDrop { inner_drop } => {
                // Deep drop: call inner type's destructor on *var before freeing
                if let Some(inner) = inner_drop {
                    let inner_entry = DropEntry {
                        var_name: format!("(*{})", entry.var_name),
                        action: (**inner).clone(),
                    };
                    Self::emit_drop_entry(&inner_entry, depth, emitter);
                }
                emitter.emit_line(&format!("free({});", entry.var_name));
            }
            DropAction::TraitObjFree { has_drop } => {
                // If the trait's vtable has a `drop` slot (trait extends Drop),
                // dispatch it through the vtable before freeing the data.
                if *has_drop {
                    emitter.emit_line(&format!(
                        "{var}.vtable->drop({var}.data);",
                        var = entry.var_name
                    ));
                }
                emitter.emit_line(&format!("free({}.data);", entry.var_name));
            }
            DropAction::FileClose => {
                emitter.emit_line(&format!("gorget_file_close(&{});", entry.var_name));
            }
            DropAction::UserDrop { type_name } => {
                let drop_fn = c_mangle::mangle_trait_method("Drop", type_name, "drop", &[]);
                emitter.emit_line(&format!("{drop_fn}(&{});", entry.var_name));
            }
            DropAction::StringFree => {
                emitter.emit_line(&format!("gorget_string_free(&{});", entry.var_name));
            }
            DropAction::ClosureEnvFree => {
                emitter.emit_line(&format!("free({}.env);", entry.var_name));
            }
            DropAction::OpaqueFree { free_fn } => {
                emitter.emit_line(&format!("{free_fn}(&{});", entry.var_name));
            }
            DropAction::StructDrop { type_name, has_user_drop, field_drops } => {
                if *has_user_drop {
                    let drop_fn = c_mangle::mangle_trait_method("Drop", type_name, "drop", &[]);
                    let v = &entry.var_name;
                    // Skip user Drop if struct was move-zeroed (all bytes zero)
                    emitter.emit_line(&format!("{{ __typeof__({v}) __zg = {{0}};"));
                    emitter.emit_line(&format!("if (memcmp(&{v}, &__zg, sizeof({v})) != 0) {{"));
                    emitter.indent();
                    emitter.emit_line(&format!("{drop_fn}(&{v});"));
                    emitter.dedent();
                    emitter.emit_line("}");
                    emitter.emit_line("}");
                }
                // Drop fields in reverse declaration order (last declared → first dropped)
                for (field_name, field_action) in field_drops.iter().rev() {
                    let field_entry = DropEntry {
                        var_name: format!("{}.{}", entry.var_name, field_name),
                        action: field_action.clone(),
                    };
                    Self::emit_drop_entry(&field_entry, depth, emitter);
                }
            }
            DropAction::ArrayFree { element_drop, element_c_type } => {
                // Drop each element before freeing the buffer
                if let (Some(edrop), Some(ec_type)) = (element_drop, element_c_type) {
                    let v = &entry.var_name;
                    emitter.emit_line(&format!("for (size_t {idx} = 0; {idx} < {v}.len; {idx}++) {{"));
                    emitter.indent();
                    emitter.emit_line(&format!(
                        "{ec_type}* {elem} = ({ec_type}*)((char*){v}.data + {idx} * sizeof({ec_type}));"
                    ));
                    let elem_entry = DropEntry {
                        var_name: format!("(*{elem})"),
                        action: (**edrop).clone(),
                    };
                    Self::emit_drop_entry(&elem_entry, depth + 1, emitter);
                    emitter.dedent();
                    emitter.emit_line("}");
                }
                emitter.emit_line(&format!("gorget_array_free(&{});", entry.var_name));
            }
            DropAction::MapFree { mangled_name, key_drop, value_drop } => {
                // Drop keys/values before freeing the map buffer
                if key_drop.is_some() || value_drop.is_some() {
                    let v = &entry.var_name;
                    emitter.emit_line(&format!("for (size_t {idx} = 0; {idx} < {v}.cap; {idx}++) {{"));
                    emitter.indent();
                    emitter.emit_line(&format!("if ({v}.states[{idx}] == 1) {{"));
                    emitter.indent();
                    if let Some(kd) = key_drop {
                        let key_entry = DropEntry {
                            var_name: format!("{v}.keys[{idx}]"),
                            action: (**kd).clone(),
                        };
                        Self::emit_drop_entry(&key_entry, depth + 1, emitter);
                    }
                    if let Some(vd) = value_drop {
                        let val_entry = DropEntry {
                            var_name: format!("{v}.values[{idx}]"),
                            action: (**vd).clone(),
                        };
                        Self::emit_drop_entry(&val_entry, depth + 1, emitter);
                    }
                    emitter.dedent();
                    emitter.emit_line("}");
                    emitter.dedent();
                    emitter.emit_line("}");
                }
                emitter.emit_line(&format!("{mangled_name}__free(&{});", entry.var_name));
            }
            DropAction::SetFree { element_drop, element_c_type } => {
                // Drop each element before freeing the set buffer
                if let (Some(edrop), Some(ec_type)) = (element_drop, element_c_type) {
                    let v = &entry.var_name;
                    emitter.emit_line(&format!("for (size_t {idx} = 0; {idx} < {v}.cap; {idx}++) {{"));
                    emitter.indent();
                    emitter.emit_line(&format!("if ({v}.states[{idx}] == 1) {{"));
                    emitter.indent();
                    emitter.emit_line(&format!(
                        "{ec_type}* {elem} = ({ec_type}*)((char*){v}.keys + {idx} * sizeof({ec_type}));"
                    ));
                    let elem_entry = DropEntry {
                        var_name: format!("(*{elem})"),
                        action: (**edrop).clone(),
                    };
                    Self::emit_drop_entry(&elem_entry, depth + 1, emitter);
                    emitter.dedent();
                    emitter.emit_line("}");
                    emitter.dedent();
                    emitter.emit_line("}");
                }
                emitter.emit_line(&format!("gorget_set_free(&{});", entry.var_name));
            }
        }
    }

    /// Generate inline C drop statements as a `String`, mirroring `emit_drop_entry()`
    /// but without `CEmitter`. Used by `compute_predrop_code()` for inline pre-drop
    /// before collection overwrites.
    ///
    /// Uses `__pdi{depth}`/`__pde{depth}` for unique loop variables (prefix `p` for
    /// "pre-drop" to avoid collision with scope-exit drop variables `__di`/`__de`).
    fn gen_inline_drop_code(var: &str, action: &DropAction, depth: usize) -> String {
        let idx = format!("__pdi{depth}");
        let elem = format!("__pde{depth}");
        match action {
            DropAction::StringFree => {
                format!("gorget_string_free(&{var});")
            }
            DropAction::ArrayFree { element_drop, element_c_type } => {
                let mut code = String::new();
                if let (Some(edrop), Some(ec_type)) = (element_drop, element_c_type) {
                    code.push_str(&format!(
                        "for (size_t {idx} = 0; {idx} < {var}.len; {idx}++) {{ \
                         {ec_type}* {elem} = ({ec_type}*)((char*){var}.data + {idx} * sizeof({ec_type})); "
                    ));
                    code.push_str(&Self::gen_inline_drop_code(&format!("(*{elem})"), edrop, depth + 1));
                    code.push_str(" } ");
                }
                code.push_str(&format!("gorget_array_free(&{var});"));
                code
            }
            DropAction::MapFree { mangled_name, key_drop, value_drop } => {
                let mut code = String::new();
                if key_drop.is_some() || value_drop.is_some() {
                    code.push_str(&format!(
                        "for (size_t {idx} = 0; {idx} < {var}.cap; {idx}++) {{ \
                         if ({var}.states[{idx}] == 1) {{ "
                    ));
                    if let Some(kd) = key_drop {
                        code.push_str(&Self::gen_inline_drop_code(
                            &format!("{var}.keys[{idx}]"), kd, depth + 1,
                        ));
                        code.push(' ');
                    }
                    if let Some(vd) = value_drop {
                        code.push_str(&Self::gen_inline_drop_code(
                            &format!("{var}.values[{idx}]"), vd, depth + 1,
                        ));
                        code.push(' ');
                    }
                    code.push_str("} } ");
                }
                code.push_str(&format!("{mangled_name}__free(&{var});"));
                code
            }
            DropAction::SetFree { element_drop, element_c_type } => {
                let mut code = String::new();
                if let (Some(edrop), Some(ec_type)) = (element_drop, element_c_type) {
                    code.push_str(&format!(
                        "for (size_t {idx} = 0; {idx} < {var}.cap; {idx}++) {{ \
                         if ({var}.states[{idx}] == 1) {{ \
                         {ec_type}* {elem} = ({ec_type}*)((char*){var}.keys + {idx} * sizeof({ec_type})); "
                    ));
                    code.push_str(&Self::gen_inline_drop_code(&format!("(*{elem})"), edrop, depth + 1));
                    code.push_str(" } } ");
                }
                code.push_str(&format!("gorget_set_free(&{var});"));
                code
            }
            DropAction::StructDrop { type_name, has_user_drop, field_drops } => {
                let mut code = String::new();
                if *has_user_drop {
                    let drop_fn = c_mangle::mangle_trait_method("Drop", type_name, "drop", &[]);
                    // Skip user Drop if struct was move-zeroed (all bytes zero)
                    code.push_str(&format!(
                        "{{ __typeof__({var}) __zg = {{0}}; \
                         if (memcmp(&{var}, &__zg, sizeof({var})) != 0) {{ \
                         {drop_fn}(&{var}); }} }} "
                    ));
                }
                // Drop fields in reverse declaration order
                for (field_name, field_action) in field_drops.iter().rev() {
                    let field_var = format!("{var}.{field_name}");
                    code.push_str(&Self::gen_inline_drop_code(&field_var, field_action, depth));
                    code.push(' ');
                }
                code
            }
            DropAction::UserDrop { type_name } => {
                let drop_fn = c_mangle::mangle_trait_method("Drop", type_name, "drop", &[]);
                format!("{drop_fn}(&{var});")
            }
            DropAction::BoxDrop { inner_drop } => {
                let mut code = String::new();
                if let Some(inner) = inner_drop {
                    code.push_str(&Self::gen_inline_drop_code(&format!("(*{var})"), inner, depth));
                    code.push(' ');
                }
                code.push_str(&format!("free({var});"));
                code
            }
            // FileClose, ClosureEnvFree, TraitObjFree — not applicable for collection elements
            _ => String::new(),
        }
    }

    /// Coerce a GorgetString C expression to `const char*`.
    ///
    /// If the expression is a named variable, field access, or dereference
    /// (already managed by the drop system), we simply return `expr.data`.
    ///
    /// If it's a function call result (temporary), we generate a named temp
    /// variable and register it in `string_temps` for materialization before
    /// the containing statement.
    ///
    /// Heuristic: an expression is a function call if it contains `(` but
    /// does NOT start with `(` (which would be a dereference, cast, or
    /// parenthesized expression).
    pub fn coerce_string_to_str(&mut self, expr: &str) -> String {
        let is_func_call = expr.contains('(') && !expr.starts_with('(');
        if !is_func_call {
            return format!("{expr}.data");
        }
        // Temporary expression — materialize to avoid leaking the GorgetString wrapper
        let id = self.string_temp_counter;
        self.string_temp_counter += 1;
        let temp_name = format!("__strtmp_{id}");
        self.string_temps.push((temp_name.clone(), expr.to_string()));
        format!("{temp_name}.data")
    }

    /// Flush pending GorgetString temporaries: emit declarations before the
    /// containing statement. Returns the temp names so the caller can emit
    /// inline cleanup after the statement.
    ///
    /// Does NOT register in the drop scope — temps are short-lived and freed
    /// inline by the statement handler. This avoids scope mismatches when
    /// statements are inside nested C blocks (if/for/match bodies).
    pub fn flush_string_temps(&mut self, emitter: &mut CEmitter) -> Vec<String> {
        let temps: Vec<(String, String)> = self.string_temps.drain(..).collect();
        let names: Vec<String> = temps.iter().map(|(n, _)| n.clone()).collect();
        for (name, expr) in temps {
            emitter.emit_line(&format!("GorgetString {name} = {expr};"));
        }
        names
    }

    /// Drain pending string temps without emitting declarations.
    /// Returns (name, expr) pairs for the caller to handle manually.
    fn flush_string_temps_no_emit(&mut self) -> Vec<(String, String)> {
        self.string_temps.drain(..).collect()
    }

    /// Emit gorget_string_free for a list of temp names (inline cleanup).
    fn emit_string_temp_frees(names: &[String], emitter: &mut CEmitter) {
        for name in names.iter().rev() {
            emitter.emit_line(&format!("gorget_string_free(&{name});"));
        }
    }

    /// Check if a trait (by name) has a `drop` method, either directly or
    /// inherited from a parent trait (i.e. the trait extends Drop).
    fn trait_has_drop_in_hierarchy(&self, trait_name: &str) -> bool {
        for ti in self.traits.traits.values() {
            if ti.name == trait_name {
                // Check own methods
                if ti.methods.contains_key("drop") {
                    return true;
                }
                // Check parent traits
                for &parent_def_id in &ti.extends {
                    if let Some(parent_info) = self.traits.traits.get(&parent_def_id) {
                        if parent_info.name == "Drop" || parent_info.methods.contains_key("drop") {
                            return true;
                        }
                    }
                }
                return false;
            }
        }
        false
    }

    /// Determine the `DropAction` for an AST `Type`, or `None` if the type is Copy.
    ///
    /// Handles primitives, String, Box (with deep drops), File, collections
    /// (Vector, Dict, HashMap, Set, HashSet), and recursively handles named
    /// structs by checking for explicit Drop impls and/or non-Copy fields.
    pub(super) fn drop_action_for_type(&self, ty: &Type) -> Option<DropAction> {
        match ty {
            Type::Primitive(crate::parser::ast::PrimitiveType::StringType) => {
                Some(DropAction::StringFree)
            }
            Type::Named { name, generic_args }
                if name.node == "Box" && !generic_args.is_empty() =>
            {
                // Check if Box[Trait] (trait object) vs Box[T] (plain pointer)
                let is_trait_obj = if let Some(Type::Named { name: inner, generic_args: inner_args }) =
                    generic_args.first().map(|a| &a.node)
                {
                    if inner_args.is_empty() {
                        self.scopes.lookup(&inner.node).map_or(false, |def_id| {
                            self.scopes.get_def(def_id).kind == crate::semantic::scope::DefKind::Trait
                        })
                    } else {
                        false
                    }
                } else {
                    false
                };
                if is_trait_obj {
                    let has_drop = if let Some(Type::Named { name: inner, .. }) =
                        generic_args.first().map(|a| &a.node)
                    {
                        self.trait_has_drop_in_hierarchy(&inner.node)
                    } else {
                        false
                    };
                    Some(DropAction::TraitObjFree { has_drop })
                } else {
                    // Deep drop: recursively check if inner type needs cleanup
                    let inner_drop = self.drop_action_for_type(&generic_args[0].node)
                        .map(|a| Box::new(a));
                    Some(DropAction::BoxDrop { inner_drop })
                }
            }
            // Vector/List/Array → buffer free + element drops
            Type::Named { name, generic_args }
                if matches!(name.node.as_str(), "Vector" | "List" | "Array")
                    && generic_args.len() == 1 =>
            {
                let elem_drop = self.drop_action_for_type(&generic_args[0].node)
                    .map(Box::new);
                let element_c_type = elem_drop.as_ref().map(|_| c_types::ast_type_to_c(&generic_args[0].node, self.scopes));
                Some(DropAction::ArrayFree { element_drop: elem_drop, element_c_type })
            }
            // Dict → monomorphized free + key/value drops
            Type::Named { name, generic_args }
                if name.node == "Dict" && generic_args.len() == 2 =>
            {
                let c_args: Vec<String> = generic_args.iter()
                    .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                    .collect();
                let mangled = c_mangle::mangle_generic("GorgetDict", &c_args);
                let key_drop = self.drop_action_for_type(&generic_args[0].node)
                    .map(Box::new);
                let value_drop = self.drop_action_for_type(&generic_args[1].node)
                    .map(Box::new);
                Some(DropAction::MapFree { mangled_name: mangled, key_drop, value_drop })
            }
            // HashMap → monomorphized free + key/value drops
            Type::Named { name, generic_args }
                if name.node == "HashMap" && generic_args.len() == 2 =>
            {
                let c_args: Vec<String> = generic_args.iter()
                    .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                    .collect();
                let mangled = c_mangle::mangle_generic("GorgetMap", &c_args);
                let key_drop = self.drop_action_for_type(&generic_args[0].node)
                    .map(Box::new);
                let value_drop = self.drop_action_for_type(&generic_args[1].node)
                    .map(Box::new);
                Some(DropAction::MapFree { mangled_name: mangled, key_drop, value_drop })
            }
            // Set/HashSet → generic set free + element drops
            Type::Named { name, generic_args }
                if matches!(name.node.as_str(), "Set" | "HashSet")
                    && generic_args.len() == 1 =>
            {
                let elem_drop = self.drop_action_for_type(&generic_args[0].node)
                    .map(Box::new);
                let element_c_type = elem_drop.as_ref().map(|_| c_types::ast_type_to_c(&generic_args[0].node, self.scopes));
                Some(DropAction::SetFree { element_drop: elem_drop, element_c_type })
            }
            Type::Named { name, generic_args }
                if name.node == "File" && generic_args.is_empty() =>
            {
                Some(DropAction::FileClose)
            }
            Type::Named { name, generic_args }
                if name.node == "Regex" && generic_args.is_empty() =>
            {
                Some(DropAction::OpaqueFree { free_fn: "gorget_regex_free".into() })
            }
            Type::Named { name, generic_args }
                if name.node == "Match" && generic_args.is_empty() =>
            {
                Some(DropAction::OpaqueFree { free_fn: "gorget_regex_match_free".into() })
            }
            Type::Named { name, generic_args } if generic_args.is_empty() => {
                // Check if this is a struct (not an enum, trait, etc.)
                let is_struct = self.scopes.lookup(&name.node).map_or(false, |def_id| {
                    self.scopes.get_def(def_id).kind == crate::semantic::scope::DefKind::Struct
                });
                if !is_struct {
                    return None;
                }

                let has_user_drop = self.traits.has_trait_impl_by_name(&name.node, "Drop");
                let field_drops = self.compute_field_drops(&name.node);

                if has_user_drop || !field_drops.is_empty() {
                    Some(DropAction::StructDrop {
                        type_name: name.node.clone(),
                        has_user_drop,
                        field_drops,
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Determine the `DropAction` for a `TypeId`, or `None` if the type is Copy.
    ///
    /// Parallel to `drop_action_for_type()` but operates on resolved TypeIds
    /// instead of AST Types. Used by `compute_predrop_code()` to produce fully
    /// recursive drop trees for collection elements.
    fn drop_action_for_type_id(&self, tid: crate::semantic::ids::TypeId) -> Option<DropAction> {
        use crate::semantic::types::ResolvedType;
        use crate::parser::ast::PrimitiveType;
        match self.types.get(tid) {
            ResolvedType::Primitive(PrimitiveType::StringType) => Some(DropAction::StringFree),
            ResolvedType::Primitive(_) => None,
            ResolvedType::Generic(def_id, args) => {
                let name = self.scopes.get_def(*def_id).name.clone();
                match name.as_str() {
                    "Vector" | "List" | "Array" if !args.is_empty() => {
                        let elem_drop = self.drop_action_for_type_id(args[0]).map(Box::new);
                        let element_c_type = elem_drop.as_ref().map(|_| self.type_id_to_c_substituted(args[0]));
                        Some(DropAction::ArrayFree { element_drop: elem_drop, element_c_type })
                    }
                    "Dict" if args.len() == 2 => {
                        let c_args: Vec<String> = args.iter().map(|&a| self.type_id_to_c_substituted(a)).collect();
                        let mangled = c_mangle::mangle_generic("GorgetDict", &c_args);
                        let key_drop = self.drop_action_for_type_id(args[0]).map(Box::new);
                        let value_drop = self.drop_action_for_type_id(args[1]).map(Box::new);
                        Some(DropAction::MapFree { mangled_name: mangled, key_drop, value_drop })
                    }
                    "HashMap" if args.len() == 2 => {
                        let c_args: Vec<String> = args.iter().map(|&a| self.type_id_to_c_substituted(a)).collect();
                        let mangled = c_mangle::mangle_generic("GorgetMap", &c_args);
                        let key_drop = self.drop_action_for_type_id(args[0]).map(Box::new);
                        let value_drop = self.drop_action_for_type_id(args[1]).map(Box::new);
                        Some(DropAction::MapFree { mangled_name: mangled, key_drop, value_drop })
                    }
                    "Set" | "HashSet" if !args.is_empty() => {
                        let elem_drop = self.drop_action_for_type_id(args[0]).map(Box::new);
                        let element_c_type = elem_drop.as_ref().map(|_| self.type_id_to_c_substituted(args[0]));
                        Some(DropAction::SetFree { element_drop: elem_drop, element_c_type })
                    }
                    "Box" if !args.is_empty() => {
                        // Check if Box[Trait] (trait object)
                        if let ResolvedType::TraitObject(trait_def_id) = self.types.get(args[0]) {
                            let has_drop = {
                                let trait_name = self.scopes.get_def(*trait_def_id).name.clone();
                                self.trait_has_drop_in_hierarchy(&trait_name)
                            };
                            Some(DropAction::TraitObjFree { has_drop })
                        } else {
                            let inner_drop = self.drop_action_for_type_id(args[0]).map(Box::new);
                            Some(DropAction::BoxDrop { inner_drop })
                        }
                    }
                    _ => None,
                }
            }
            ResolvedType::Defined(def_id) => {
                let name = self.scopes.get_def(*def_id).name.clone();
                self.drop_action_for_struct_name(&name)
            }
            _ => None,
        }
    }

    /// Compute the drop action for a struct given its C type name.
    /// Used for auto-typed variables where `drop_action_for_type` returns None
    /// because the AST type is Inferred.
    fn drop_action_for_struct_name(&self, c_type_name: &str) -> Option<DropAction> {
        let is_struct = self.scopes.lookup(c_type_name).map_or(false, |def_id| {
            self.scopes.get_def(def_id).kind == crate::semantic::scope::DefKind::Struct
        });
        if !is_struct {
            return None;
        }
        let has_user_drop = self.traits.has_trait_impl_by_name(c_type_name, "Drop");
        let field_drops = self.compute_field_drops(c_type_name);
        if has_user_drop || !field_drops.is_empty() {
            Some(DropAction::StructDrop {
                type_name: c_type_name.to_string(),
                has_user_drop,
                field_drops,
            })
        } else {
            None
        }
    }

    /// Determine the `DropAction` from a mangled C type string.
    ///
    /// Used for async state struct cleanup where only the C type string is available
    /// (e.g., inferred-type locals). For explicitly-typed locals, `drop_action_for_type()`
    /// gives full recursive drops including collection element drops.
    pub(super) fn drop_action_for_c_type(&self, c_type: &str) -> Option<DropAction> {
        match c_type {
            "GorgetString" => Some(DropAction::StringFree),
            ct if ct.starts_with("GorgetArray__") => {
                Some(DropAction::ArrayFree { element_drop: None, element_c_type: None })
            }
            ct if ct.starts_with("GorgetDict__") || ct.starts_with("GorgetMap__") => {
                Some(DropAction::MapFree { mangled_name: ct.to_string(), key_drop: None, value_drop: None })
            }
            ct if ct.starts_with("GorgetSet__") => {
                Some(DropAction::SetFree { element_drop: None, element_c_type: None })
            }
            // Primitives, Future/Task, Channel — no drop
            "int64_t" | "double" | "bool" | "char" | "const char*"
            | "uint8_t" | "uint16_t" | "uint32_t" | "uint64_t" | "size_t" | "void" => None,
            ct if ct.starts_with("Future__") || ct.starts_with("Task__")
                || ct.starts_with("GorgetChannel") => None,
            // Pointer types — no drop (borrowed)
            ct if ct.ends_with('*') => None,
            // User struct: look up by name
            ct => self.drop_action_for_struct_name(ct),
        }
    }

    /// Compute per-field drop actions for a struct by name.
    ///
    /// Returns `(escaped_field_name, DropAction)` pairs for each non-Copy field.
    /// Uses `struct_fields` for field names and `field_type_names` for field types.
    fn compute_field_drops(&self, struct_name: &str) -> Vec<(String, DropAction)> {
        // Look up the struct's DefId
        let def_id = match self.scopes.lookup(struct_name) {
            Some(id) => id,
            None => return Vec::new(),
        };

        // Get field names from struct_fields
        let field_info = match self.struct_fields.get(&def_id) {
            Some(info) => info,
            None => return Vec::new(),
        };

        let mut drops = Vec::new();
        for (field_name, _span) in &field_info.fields {
            let key = (struct_name.to_string(), field_name.clone());
            if let Some(field_type) = self.field_type_names.get(&key) {
                if let Some(action) = self.drop_action_for_type(field_type) {
                    let escaped = c_mangle::escape_keyword(field_name);
                    drops.push((escaped, action));
                }
            }
        }
        drops
    }

    /// Emit `memset` zeroing for variables that were moved in the current statement.
    /// After zeroing, the scope-exit drop becomes a no-op (e.g. `free(NULL)` is safe).
    pub fn flush_move_zeros(&mut self, emitter: &mut CEmitter) {
        for var in self.pending_move_zeros.drain(..) {
            emitter.emit_line(&format!("memset(&{var}, 0, sizeof({var}));"));
        }
    }

    /// Find the DropAction for a variable in any active drop scope.
    pub fn find_drop_action(&self, var_name: &str) -> Option<DropAction> {
        for (_kind, entries) in self.drop_scopes.iter().rev() {
            for entry in entries {
                if entry.var_name == var_name {
                    return Some(entry.action.clone());
                }
            }
        }
        None
    }

    /// Queue a variable for move-zeroing if it has a registered drop action.
    /// After the statement completes, `flush_move_zeros()` emits `memset(&var, 0, sizeof(var))`,
    /// making the scope-exit drop a no-op (`free(NULL)` is safe per C standard).
    pub fn queue_move_zero_if_droppable(&mut self, name: &str) {
        let escaped = c_mangle::escape_keyword(name);
        if self.find_drop_action(&escaped).is_some() {
            self.pending_move_zeros.push(escaped);
        }
    }

    /// Queue a struct field for move-zeroing if the field is non-Copy.
    /// Returns true if zeroing was queued, false if skipped.
    ///
    /// Safety gates:
    /// - Object must resolve via `resolve_field_zero_target` (Identifier, SelfExpr, or chained FieldAccess)
    /// - Object must not be a pointer parameter
    /// - Parent struct must NOT have user Drop (partial zeroing → UB in user Drop)
    /// - Field type must be non-Copy (has a drop action)
    pub(super) fn queue_field_move_zero(&mut self, object: &Expr, field_name: &str) -> bool {
        let (obj_path, obj_type, is_ptr) = match self.resolve_field_zero_target(object) {
            Some(t) => t,
            None => return false,
        };
        let key = (obj_type, field_name.to_string());
        if let Some(field_type) = self.field_type_names.get(&key) {
            if self.drop_action_for_type(field_type).is_some() {
                let escaped_field = c_mangle::escape_keyword(field_name);
                let accessor = if is_ptr { "->" } else { "." };
                self.pending_move_zeros.push(format!("{obj_path}{accessor}{escaped_field}"));
                return true;
            }
        }
        false
    }

    /// Resolve the target of a field-level move-zero: returns `(c_path, struct_type_name, is_pointer)`.
    ///
    /// Handles three cases:
    /// - `Identifier`: local variable with StructDrop (no user Drop)
    /// - `SelfExpr`: `self` pointer in a mutable equip method (no user Drop)
    /// - `FieldAccess`: chained access `obj.a.b` — recursively resolves the inner object
    fn resolve_field_zero_target(&self, object: &Expr) -> Option<(String, String, bool)> {
        match object {
            Expr::Identifier(name) => {
                let escaped = c_mangle::escape_keyword(name);
                if self.pointer_params.contains(&escaped) {
                    return None;
                }
                match self.find_drop_action(&escaped) {
                    Some(DropAction::StructDrop { has_user_drop: false, type_name, .. }) => {
                        Some((escaped, type_name, false))
                    }
                    _ => None,
                }
            }
            Expr::SelfExpr => {
                if !self.self_is_mutable {
                    return None;
                }
                let self_type = self.current_self_type.as_ref()?;
                if self.traits.has_trait_impl_by_name(self_type, "Drop") {
                    return None;
                }
                Some(("self".to_string(), self_type.clone(), true))
            }
            Expr::FieldAccess { object: inner, field } => {
                let (parent_path, parent_type, parent_is_ptr) =
                    self.resolve_field_zero_target(&inner.node)?;
                let key = (parent_type, field.node.clone());
                let field_type = self.field_type_names.get(&key)?;
                // Must be a plain Named struct type (no generics)
                let struct_name = match field_type {
                    Type::Named { name, generic_args } if generic_args.is_empty() => &name.node,
                    _ => return None,
                };
                // Must be a struct, must NOT have user Drop
                let is_struct = self.scopes.lookup(struct_name).map_or(false, |def_id| {
                    self.scopes.get_def(def_id).kind == crate::semantic::scope::DefKind::Struct
                });
                if !is_struct {
                    return None;
                }
                if self.traits.has_trait_impl_by_name(struct_name, "Drop") {
                    return None;
                }
                let escaped_field = c_mangle::escape_keyword(&field.node);
                let accessor = if parent_is_ptr { "->" } else { "." };
                Some((format!("{parent_path}{accessor}{escaped_field}"), struct_name.clone(), false))
            }
            _ => None,
        }
    }

    /// Try to queue field-level move-zeroing for a FieldAccess expression.
    /// Returns true if zeroing was queued.
    fn try_queue_field_move_zero_for_value(&mut self, expr: &Expr) -> bool {
        if let Expr::FieldAccess { object, field } = expr {
            self.queue_field_move_zero(&object.node, &field.node)
        } else {
            false
        }
    }

    /// Check whether a source expression is safe to push without deep-cloning.
    /// Safe when:
    /// - Identifier with a registered drop (will be move-zeroed, sole owner)
    /// - FieldAccess with valid zero target (field will be zeroed, sole owner)
    /// - Fresh allocation: Call/constructor/method return (uniquely owned, no aliasing)
    pub(super) fn source_is_safe_for_push(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(name) => {
                let escaped = c_mangle::escape_keyword(name);
                self.find_drop_action(&escaped).is_some()
            }
            Expr::FieldAccess { object, .. } => {
                self.resolve_field_zero_target(&object.node).is_some()
            }
            _ => Self::is_fresh_allocation_source(expr),
        }
    }

    /// Returns clone actions for fields of a struct that need deep-cloning.
    pub(super) fn compute_clone_fields(&self, struct_name: &str) -> Vec<(String, super::CloneAction)> {
        let def_id = match self.scopes.lookup(struct_name) {
            Some(id) => id,
            None => return Vec::new(),
        };
        let field_info = match self.struct_fields.get(&def_id) {
            Some(info) => info,
            None => return Vec::new(),
        };

        let mut clones = Vec::new();
        for (field_name, _span) in &field_info.fields {
            let key = (struct_name.to_string(), field_name.clone());
            if let Some(field_type) = self.field_type_names.get(&key) {
                if let Some(action) = self.clone_action_for_type(field_type) {
                    let escaped = c_mangle::escape_keyword(field_name);
                    clones.push((escaped, action));
                }
            }
        }
        clones
    }

    /// Determine the CloneAction for a given AST type, if it needs cloning.
    fn clone_action_for_type(&self, ty: &Type) -> Option<super::CloneAction> {
        match ty {
            Type::Named { name, generic_args } => match name.node.as_str() {
                "Vector" | "List" | "Array" if generic_args.len() == 1 => {
                    // Check if element type is a struct with non-Copy fields
                    // that also need deep cloning.
                    let elem_type = &generic_args[0].node;
                    if let Type::Named { name: elem_name, generic_args: elem_generics } = elem_type {
                        if elem_generics.is_empty() {
                            let elem_clone_fields = self.compute_clone_fields(&elem_name.node);
                            if !elem_clone_fields.is_empty() {
                                let elem_c_type = c_types::ast_type_to_c(elem_type, self.scopes);
                                return Some(super::CloneAction::ArrayDeepClone(
                                    elem_c_type, elem_clone_fields
                                ));
                            }
                        }
                    }
                    Some(super::CloneAction::ArrayClone)
                }
                "String" if generic_args.is_empty() => {
                    Some(super::CloneAction::StringClone)
                }
                "Dict" if generic_args.len() == 2 => {
                    let c_args: Vec<String> = generic_args.iter()
                        .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                        .collect();
                    let mangled = c_mangle::mangle_generic("GorgetDict", &c_args);
                    Some(super::CloneAction::MapClone(mangled))
                }
                "HashMap" if generic_args.len() == 2 => {
                    let c_args: Vec<String> = generic_args.iter()
                        .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                        .collect();
                    let mangled = c_mangle::mangle_generic("GorgetMap", &c_args);
                    Some(super::CloneAction::MapClone(mangled))
                }
                "Set" | "HashSet" if generic_args.len() == 1 => {
                    Some(super::CloneAction::SetClone)
                }
                _ => {
                    // Check if this is a struct with clonable fields
                    if generic_args.is_empty() {
                        let struct_name = &name.node;
                        let is_struct = self.scopes.lookup(struct_name).map_or(false, |def_id| {
                            self.scopes.get_def(def_id).kind == crate::semantic::scope::DefKind::Struct
                        });
                        if is_struct {
                            let clone_fields = self.compute_clone_fields(struct_name);
                            if !clone_fields.is_empty() {
                                return Some(super::CloneAction::StructClone(clone_fields));
                            }
                        }
                    }
                    None
                }
            }
            _ => None,
        }
    }

    /// Generate clone code for a single `CloneAction`, recursing into `StructClone`.
    fn gen_clone_code_for_action(var: &str, action: &super::CloneAction) -> String {
        match action {
            super::CloneAction::ArrayClone => {
                format!("{var} = gorget_array_clone(&{var});")
            }
            super::CloneAction::ArrayDeepClone(elem_c_type, elem_field_clones) => {
                // Clone the array, then iterate elements and deep-clone their fields.
                let elem_clones: Vec<String> = elem_field_clones.iter().map(|(field, act)| {
                    let field_expr = format!("(*__dce).{field}");
                    Self::gen_clone_code_for_action(&field_expr, act)
                }).collect();
                format!(
                    "{var} = gorget_array_clone(&{var}); \
                    for (size_t __dci = 0; __dci < {var}.len; __dci++) {{ \
                    {elem_c_type}* __dce = ({elem_c_type}*)((char*){var}.data + __dci * sizeof({elem_c_type})); \
                    {elem_clone} }}",
                    elem_clone = elem_clones.join(" ")
                )
            }
            super::CloneAction::StringClone => {
                format!("{var} = gorget_string_clone(&{var});")
            }
            super::CloneAction::MapClone(mangled) => {
                format!("{var} = {mangled}__clone(&{var});")
            }
            super::CloneAction::SetClone => {
                format!("{var} = gorget_set_clone(&{var});")
            }
            super::CloneAction::StructClone(field_clones) => {
                let lines: Vec<String> = field_clones.iter().map(|(field, act)| {
                    let field_expr = format!("{var}.{field}");
                    Self::gen_clone_code_for_action(&field_expr, act)
                }).collect();
                lines.join(" ")
            }
        }
    }

    /// Returns clone code for a value being pushed into a collection.
    /// Handles both collection-typed values (GorgetArray, GorgetString, etc.)
    /// and struct-typed values with collection fields.
    pub(super) fn compute_push_clone_code(&self, elem_c_type: &str, var: &str) -> Option<String> {
        // Direct collection types
        if elem_c_type == "GorgetArray" {
            return Some(format!("{var} = gorget_array_clone(&{var});"));
        }
        if elem_c_type == "GorgetString" {
            return Some(format!("{var} = gorget_string_clone(&{var});"));
        }
        if elem_c_type.starts_with("GorgetDict__") || elem_c_type.starts_with("GorgetMap__") {
            return Some(format!("{var} = {elem_c_type}__clone(&{var});"));
        }
        if elem_c_type == "GorgetSet" {
            return Some(format!("{var} = gorget_set_clone(&{var});"));
        }

        // Struct types: check if any fields need cloning
        // Look up the struct name from the C type — it's the same since
        // non-generic struct names are used directly in C
        let struct_name = elem_c_type;
        let clone_fields = self.compute_clone_fields(struct_name);
        if clone_fields.is_empty() {
            return None;
        }

        let action = super::CloneAction::StructClone(clone_fields);
        Some(Self::gen_clone_code_for_action(var, &action))
    }

    /// Compute inline pre-drop code for an element about to be overwritten
    /// in a collection. Parallel to `compute_push_clone_code()`.
    ///
    /// Returns `Some(code)` if the element type is non-Copy and needs cleanup
    /// before overwrite, `None` if Copy (no cleanup needed).
    ///
    /// When `elem_type_id` is `Some`, uses `drop_action_for_type_id()` for full
    /// recursive element drops (e.g., freeing String elements inside a nested
    /// Vector before freeing the Vector buffer). Falls back to buffer-only
    /// cleanup when `None`.
    pub(super) fn compute_predrop_code(
        &self,
        elem_c_type: &str,
        var: &str,
        elem_type_id: Option<crate::semantic::ids::TypeId>,
    ) -> Option<String> {
        // When we have full type info, use it for recursive drops on collection types
        if let Some(tid) = elem_type_id {
            if elem_c_type == "GorgetArray"
                || elem_c_type.starts_with("GorgetDict__")
                || elem_c_type.starts_with("GorgetMap__")
                || elem_c_type == "GorgetSet"
            {
                if let Some(action) = self.drop_action_for_type_id(tid) {
                    return Some(Self::gen_inline_drop_code(var, &action, 0));
                }
            }
        }

        // Direct collection/string types
        if elem_c_type == "GorgetString" {
            return Some(format!("gorget_string_free(&{var});"));
        }
        if elem_c_type == "GorgetArray" {
            // Buffer-only: inner elem type unknown from C string
            return Some(format!("gorget_array_free(&{var});"));
        }
        if elem_c_type.starts_with("GorgetDict__") || elem_c_type.starts_with("GorgetMap__") {
            // Buffer-only: inner types unknown from C string
            return Some(format!("{elem_c_type}__free(&{var});"));
        }
        if elem_c_type == "GorgetSet" {
            return Some(format!("gorget_set_free(&{var});"));
        }

        // Struct types: full StructDrop with user drop + recursive field drops
        self.drop_action_for_struct_name(elem_c_type)
            .map(|action| Self::gen_inline_drop_code(var, &action, 0))
    }

    /// Remove all drop entries for `var_name` across all scopes.
    /// Used to transfer ownership of a closure env when returning it.
    pub fn remove_droppable(&mut self, var_name: &str) {
        for (_kind, entries) in &mut self.drop_scopes {
            entries.retain(|e| e.var_name != var_name);
        }
    }

    /// Scan a function body's statements for closure variables that escape
    /// (are returned). Returns the set of variable names whose closure envs
    /// should be heap-allocated.
    pub fn scan_escaping_closures(&self, block: &Block) -> std::collections::HashSet<String> {
        let mut closure_candidates = std::collections::HashSet::new();
        let mut escaping = std::collections::HashSet::new();

        for stmt in &block.stmts {
            match &stmt.node {
                // auto f = (params): body  →  candidate if it has captures
                Stmt::VarDecl { pattern, value, .. } => {
                    if let Pattern::Binding(name) = &pattern.node {
                        if let Expr::Closure { params, body, .. } = &value.node {
                            let bound: std::collections::HashSet<&str> =
                                params.iter().map(|p| p.node.name.node.as_str()).collect();
                            let captures = self.collect_free_vars_readonly(body, &bound);
                            if !captures.is_empty() {
                                closure_candidates.insert(name.clone());
                            }
                        }
                    }
                }
                // return f  where f is a closure candidate  →  escaping
                Stmt::Return(Some(expr)) => {
                    if let Expr::Identifier(name) = &expr.node {
                        if closure_candidates.contains(name) {
                            escaping.insert(name.clone());
                        }
                    }
                    // return (params): body  →  inline closure literal in return
                    if matches!(&expr.node, Expr::Closure { .. }) {
                        // Mark with a sentinel so the return handler knows
                        escaping.insert("__inline_return_closure__".to_string());
                    }
                }
                _ => {}
            }
        }

        escaping
    }

    /// Read-only version of collect_free_vars that doesn't need &mut self.
    /// Returns non-empty if the closure captures variables.
    fn collect_free_vars_readonly(
        &self,
        expr: &Spanned<Expr>,
        bound: &std::collections::HashSet<&str>,
    ) -> Vec<String> {
        let mut collector = FreeVarReadonlyCollector {
            bound: bound.clone(),
            scopes: self.scopes,
            seen: std::collections::HashSet::new(),
            free: Vec::new(),
        };
        collector.visit_expr(expr);
        collector.free
    }

    /// Emit a structural trace "return" event that closes the function's Call frame.
    /// This is purely for tree-builder nesting; the visual return is shown by stmt_start.
    pub(super) fn emit_trace_return(&self, emitter: &mut CEmitter) {
        let fn_name = self.current_function_gorget_name.as_deref().unwrap_or("?");
        emitter.emit_line("__gorget_trace_depth--;");
        emitter.emit_line(&format!(
            r#"fprintf(__gorget_trace_fp, "{{\"type\":\"return\",\"fn\":\"{fn_name}\",\"depth\":%d}}\n", __gorget_trace_depth);"#
        ));
    }

    /// Coerce a return value between GorgetString and const char* when the
    /// function return type doesn't match the expression type.
    pub(super) fn coerce_return_value(&mut self, expr: String, ast_expr: &Expr) -> String {
        let ret_type = self.current_function_return_c_type.clone();
        if let Some(ret_type) = ret_type {
            let val_type = self.infer_c_type_from_expr(ast_expr);
            if ret_type == "const char*" && val_type == "GorgetString" {
                return format!("{expr}.data");
            }
            if ret_type == "GorgetString" && val_type == "const char*" {
                return format!("gorget_string_new({expr})");
            }
        }
        expr
    }

    /// Return the C type to use for `__ret_tmp`. Prefers the known function return
    /// type (avoids `__typeof__` on string literals producing `char[N]` instead of
    /// `const char*`). Falls back to `__typeof__(expr)` if return type is unknown.
    fn ret_tmp_type(&self, expr: &str) -> String {
        match &self.current_function_return_c_type {
            Some(t) => t.clone(),
            None => format!("__typeof__({expr})"),
        }
    }

    /// Walk expression ASTs collecting `Expr::Identifier` references suitable
    /// for variable substitution in trace output. Returns deduplicated
    /// `(gorget_name, c_name, formatter_fn)` triples for traceable variables.
    fn collect_expr_vars(&self, exprs: &[&Spanned<Expr>]) -> Vec<(String, String, &'static str)> {
        let mut collector = TraceVarCollector {
            scopes: self.scopes,
            types: self.types,
            current_function_scope: self.current_function_scope,
            seen: std::collections::HashSet::new(),
            out: Vec::new(),
        };
        for expr in exprs {
            collector.visit_expr(expr);
        }
        collector.out
    }

    /// Look up trace info for a single variable by name.
    /// Returns `(gorget_name, c_name, formatter)` if the variable is traceable.
    fn lookup_var_trace_info(&self, name: &str) -> Option<(String, String, &'static str)> {
        let def_id = self.scoped_lookup(name)?;
        let def = self.scopes.get_def(def_id);
        match def.kind {
            DefKind::Variable | DefKind::Const => {}
            _ => return None,
        }
        let type_id = def.type_id?;
        let c_type = c_types::type_id_to_c(type_id, self.types, self.scopes);
        if c_types::is_traceable_for_vars(&c_type) {
            let formatter = c_types::trace_formatter_for_c_type(&c_type);
            let c_name = c_mangle::escape_keyword(name);
            Some((name.to_string(), c_name, formatter))
        } else {
            None
        }
    }

    /// Emit a stmt_start trace event (before expression evaluation).
    fn emit_stmt_start(&self, span: Span, vars: &[(String, String, &str)], emitter: &mut CEmitter) {
        let src = c_string_escape(&self.source_line(span));
        emitter.emit_line(
            r#"fprintf(__gorget_trace_fp, "{\"type\":\"stmt_start\",\"src\":\"");"#
        );
        emitter.emit_line(&format!(
            r#"__gorget_trace_json_str(__gorget_trace_fp, "{src}");"#
        ));
        if vars.is_empty() {
            emitter.emit_line(
                r#"fprintf(__gorget_trace_fp, "\",\"depth\":%d}\n", __gorget_trace_depth);"#
            );
        } else {
            emitter.emit_line(
                r#"fprintf(__gorget_trace_fp, "\",\"vars\":{");"#
            );
            for (i, (gorget_name, c_name, formatter)) in vars.iter().enumerate() {
                let comma = if i > 0 { "," } else { "" };
                emitter.emit_line(&format!(
                    r#"fprintf(__gorget_trace_fp, "{comma}\"{gorget_name}\":");"#
                ));
                emitter.emit_line(&format!(
                    "{formatter}(__gorget_trace_fp, {c_name});"
                ));
            }
            emitter.emit_line(
                r#"fprintf(__gorget_trace_fp, "},\"depth\":%d}\n", __gorget_trace_depth);"#
            );
        }
        emitter.emit_line("__gorget_trace_depth++;");
    }

    /// Emit a stmt_end trace event (after expression evaluation).
    fn emit_stmt_end(&self, emitter: &mut CEmitter) {
        emitter.emit_line("__gorget_trace_depth--;");
        emitter.emit_line(
            r#"fprintf(__gorget_trace_fp, "{\"type\":\"stmt_end\",\"depth\":%d}\n", __gorget_trace_depth);"#
        );
    }

    /// Emit a stmt_end trace event with a result variable value.
    fn emit_stmt_end_with_result(
        &self,
        result_info: &(String, String, &str),
        emitter: &mut CEmitter,
    ) {
        let (gorget_name, c_name, formatter) = result_info;
        emitter.emit_line("__gorget_trace_depth--;");
        emitter.emit_line(
            r#"fprintf(__gorget_trace_fp, "{\"type\":\"stmt_end\",\"vars\":{");"#
        );
        emitter.emit_line(&format!(
            r#"fprintf(__gorget_trace_fp, "\"{gorget_name}\":");"#
        ));
        emitter.emit_line(&format!(
            "{formatter}(__gorget_trace_fp, {c_name});"
        ));
        emitter.emit_line(
            r#"fprintf(__gorget_trace_fp, "},\"depth\":%d}\n", __gorget_trace_depth);"#
        );
    }

    /// Emit a branch trace event (if/elif/else taken).
    fn emit_branch_trace(
        &self,
        kind: &str,
        src: &str,
        vars: &[(String, String, &str)],
        emitter: &mut CEmitter,
    ) {
        emitter.emit_line(&format!(
            r#"fprintf(__gorget_trace_fp, "{{\"type\":\"branch\",\"kind\":\"{kind}\",\"src\":\"");"#
        ));
        emitter.emit_line(&format!(
            r#"__gorget_trace_json_str(__gorget_trace_fp, "{src}");"#
        ));
        if vars.is_empty() {
            emitter.emit_line(
                r#"fprintf(__gorget_trace_fp, "\",\"depth\":%d}\n", __gorget_trace_depth);"#
            );
        } else {
            emitter.emit_line(
                r#"fprintf(__gorget_trace_fp, "\",\"vars\":{");"#
            );
            for (i, (gorget_name, c_name, formatter)) in vars.iter().enumerate() {
                let comma = if i > 0 { "," } else { "" };
                emitter.emit_line(&format!(
                    r#"fprintf(__gorget_trace_fp, "{comma}\"{gorget_name}\":");"#
                ));
                emitter.emit_line(&format!(
                    "{formatter}(__gorget_trace_fp, {c_name});"
                ));
            }
            emitter.emit_line(
                r#"fprintf(__gorget_trace_fp, "},\"depth\":%d}\n", __gorget_trace_depth);"#
            );
        }
    }

    /// Emit a while-loop iteration trace event.
    fn emit_while_iter_trace(&self, iter_var: &str, emitter: &mut CEmitter) {
        emitter.emit_line(
            r#"fprintf(__gorget_trace_fp, "{\"type\":\"loop\",\"kind\":\"while\",\"iter\":");"#
        );
        emitter.emit_line(&format!(
            "__gorget_trace_val_int(__gorget_trace_fp, {iter_var}++);"
        ));
        emitter.emit_line(
            r#"fprintf(__gorget_trace_fp, ",\"depth\":%d}\n", __gorget_trace_depth);"#
        );
    }

    /// Check if an init expression is a collection constructor call.
    /// Only constructor calls produce independently-owned buffers that are
    /// safe to free at scope exit. Other init expressions (function calls,
    /// copies, field access) may share buffers with existing data and must
    /// NOT have drops registered to avoid double-frees.
    ///
    /// Constructor patterns:
    /// - `Vector[T]()` / `Dict[K,V]()` / `Set[T]()` → `Call { callee: Identifier("Vector"|...) }`
    /// - `StructLiteral { name: "MyStruct", ... }` → struct with collection fields
    /// - Dict/Set/List comprehensions → new collection
    fn is_owning_collection_init(expr: &Expr) -> bool {
        match expr {
            // Collection constructor: Vector[T](), Dict[K,V](), Set[T](), etc.
            Expr::Call { callee, .. } => {
                if let Expr::Identifier(name) = &callee.node {
                    matches!(name.as_str(),
                        "Vector" | "List" | "Array" | "Dict" | "HashMap" | "Set" | "HashSet"
                    )
                } else {
                    false
                }
            }
            // Struct literal: the struct owns its collection fields
            Expr::StructLiteral { .. } => true,
            // Dict/Set/List literals and comprehensions create new collections
            Expr::DictLiteral(_) => true,
            Expr::DictComprehension { .. } => true,
            Expr::ListComprehension { .. } => true,
            Expr::SetComprehension { .. } => true,
            // Box constructor: Box(value) or Box.new(value)
            Expr::Path { .. } => false,
            _ => false,
        }
    }

    /// Does this expression produce a freshly-allocated collection (via function
    /// or method call)? These get buffer-only drops (elements may be aliased).
    /// Distinct from `is_owning_collection_init` which identifies constructors
    /// that produce fully-owned collections eligible for element-level drops.
    ///
    /// For function calls (`Expr::Call`), any non-constructor call is accepted
    /// since user functions that return collections typically construct them
    /// internally (fresh buffer).
    ///
    /// For method calls (`Expr::MethodCall`), only a whitelist of methods known
    /// to allocate fresh buffers is accepted. Methods like `.get()` and
    /// `.unwrap()` return shallow copies sharing the source's buffer — those
    /// must NOT get drops to avoid double-frees.
    fn is_fresh_allocation_source(expr: &Expr) -> bool {
        match expr {
            Expr::Call { .. } => true,
            Expr::MethodCall { method, .. } => {
                matches!(method.node.as_str(),
                    // Dict/HashMap → fresh Vector
                    "keys" | "values" | "items" |
                    // Collection → fresh collection of same/new type
                    "filter" | "map" | "sorted" | "slice" |
                    // str → fresh Vector[str]
                    "split" |
                    // Set → fresh Set
                    "union" | "intersection" | "difference"
                )
            }
            _ => false,
        }
    }

    /// Emit `__gorget_cleanup_push` calls for test-body mode.
    /// Handles leaf types directly; for StructDrop, emits per-field cleanup
    /// for known leaf types.
    fn emit_test_cleanup_for_action(var_name: &str, action: &DropAction, emitter: &mut CEmitter) {
        match action {
            DropAction::BoxDrop { .. } => {
                emitter.emit_line(&format!(
                    "__gorget_cleanup_push(free, (void*){var_name});"
                ));
            }
            DropAction::TraitObjFree { .. } => {
                emitter.emit_line(&format!(
                    "__gorget_cleanup_push(free, (void*){var_name}.data);"
                ));
            }
            DropAction::FileClose => {
                emitter.emit_line(&format!(
                    "__gorget_cleanup_push((__gorget_cleanup_fn)gorget_file_close, (void*)&{var_name});"
                ));
            }
            DropAction::UserDrop { type_name } => {
                let drop_fn = c_mangle::mangle_trait_method("Drop", type_name, "drop", &[]);
                emitter.emit_line(&format!(
                    "__gorget_cleanup_push((__gorget_cleanup_fn){drop_fn}, (void*)&{var_name});"
                ));
            }
            DropAction::StringFree => {
                emitter.emit_line(&format!(
                    "__gorget_cleanup_push((__gorget_cleanup_fn)gorget_string_free, (void*)&{var_name});"
                ));
            }
            DropAction::ClosureEnvFree => {
                emitter.emit_line(&format!(
                    "__gorget_cleanup_push(free, (void*){var_name}.env);"
                ));
            }
            DropAction::OpaqueFree { free_fn } => {
                emitter.emit_line(&format!(
                    "__gorget_cleanup_push((__gorget_cleanup_fn){free_fn}, (void*)&{var_name});"
                ));
            }
            DropAction::StructDrop { type_name, has_user_drop, field_drops } => {
                // For test-body cleanup, register the user drop if present
                if *has_user_drop {
                    let drop_fn = c_mangle::mangle_trait_method("Drop", type_name, "drop", &[]);
                    emitter.emit_line(&format!(
                        "__gorget_cleanup_push((__gorget_cleanup_fn){drop_fn}, (void*)&{var_name});"
                    ));
                }
                // Register leaf field cleanups
                for (field_name, field_action) in field_drops {
                    let field_var = format!("{var_name}.{field_name}");
                    Self::emit_test_cleanup_for_action(&field_var, field_action, emitter);
                }
            }
            DropAction::ArrayFree { .. } => {
                emitter.emit_line(&format!(
                    "__gorget_cleanup_push((__gorget_cleanup_fn)gorget_array_free, (void*)&{var_name});"
                ));
            }
            DropAction::MapFree { mangled_name, .. } => {
                emitter.emit_line(&format!(
                    "__gorget_cleanup_push((__gorget_cleanup_fn){mangled_name}__free, (void*)&{var_name});"
                ));
            }
            DropAction::SetFree { .. } => {
                emitter.emit_line(&format!(
                    "__gorget_cleanup_push((__gorget_cleanup_fn)gorget_set_free, (void*)&{var_name});"
                ));
            }
        }
    }

    /// Generate C code for a single statement.
    pub fn gen_stmt(&mut self, stmt: &Stmt, span: Span, emitter: &mut CEmitter) {
        match stmt {
            Stmt::VarDecl {
                is_const,
                type_,
                pattern,
                value,
                ..
            } => {
                self.gen_var_decl(*is_const, type_, pattern, value, span, emitter);
            }

            Stmt::Expr(expr) => {
                if self.trace {
                    let vars = self.collect_expr_vars(&[expr]);
                    self.emit_stmt_start(span, &vars, emitter);
                }
                let e = self.gen_expr(expr);
                let str_temps = self.flush_string_temps(emitter);
                emitter.emit_line(&format!("{e};"));
                self.flush_move_zeros(emitter);
                Self::emit_string_temp_frees(&str_temps, emitter);
                if self.trace {
                    self.emit_stmt_end(emitter);
                }
            }

            Stmt::Assign { target, value } => {
                // Dict/HashMap subscript write: d[key] = value → __put(&d, key, value)
                if let Expr::Index { object, index } = &target.node {
                    if self.is_gorget_map_expr(object) {
                        if self.trace {
                            let vars = self.collect_expr_vars(&[target, value]);
                            self.emit_stmt_start(span, &vars, emitter);
                        }
                        let obj = self.gen_expr(object);
                        let idx = self.gen_expr(index);
                        let val = self.gen_expr(value);
                        let mangled = self.infer_map_mangled(object);
                        emitter.emit_line(&format!("{mangled}__put(&{obj}, {idx}, {val});"));
                        if self.trace {
                            self.emit_stmt_end(emitter);
                        }
                        return;
                    }
                    // IndexMut trait dispatch: obj[key] = value → IndexMut_for_Type__set(&obj, key, value)
                    if let Some((type_name, trait_type_args)) = self.try_operator_trait_type(object, "IndexMut") {
                        if self.trace {
                            let vars = self.collect_expr_vars(&[target, value]);
                            self.emit_stmt_start(span, &vars, emitter);
                        }
                        let obj = self.gen_expr(object);
                        let idx = self.gen_expr(index);
                        let val = self.gen_expr(value);
                        let mangled = c_mangle::mangle_trait_method("IndexMut", &type_name, "set", &trait_type_args);
                        let str_temps = self.flush_string_temps(emitter);
                        emitter.emit_line(&format!("{mangled}(&{obj}, {idx}, {val});"));
                        Self::emit_string_temp_frees(&str_temps, emitter);
                        if self.trace {
                            self.emit_stmt_end(emitter);
                        }
                        return;
                    }
                }
                let result_info = if self.trace {
                    let vars = self.collect_expr_vars(&[target, value]);
                    self.emit_stmt_start(span, &vars, emitter);
                    if let Expr::Identifier(name) = &target.node {
                        self.lookup_var_trace_info(name)
                    } else {
                        None
                    }
                } else {
                    None
                };
                let t = self.gen_expr(target);
                // Set decl_type_hint so unit variant constructors (None(), Some(), Ok(), Error())
                // resolve to the correct monomorphized enum in assignment context.
                let saved_hint = self.decl_type_hint.clone();
                if let Expr::FieldAccess { object, field, .. } = &target.node {
                    let obj_type = self.infer_receiver_type(object);
                    if obj_type != "Unknown" {
                        let key = (obj_type, field.node.clone());
                        if let Some(ast_type) = self.field_type_names.get(&key).cloned() {
                            if matches!(&ast_type, Type::Named { generic_args, .. } if !generic_args.is_empty()) {
                                self.decl_type_hint = Some(ast_type);
                            }
                        }
                    }
                } else if let Some(tid) = self.resolve_expr_type_id(target) {
                    if let Some(ast_type) = self.type_id_to_ast_type(tid) {
                        if matches!(&ast_type, Type::Named { generic_args, .. } if !generic_args.is_empty()) {
                            self.decl_type_hint = Some(ast_type);
                        }
                    }
                }
                let v = self.gen_expr(value);
                self.decl_type_hint = saved_hint;
                // Coerce between String and str on assignment
                let target_type = self.infer_c_type_from_expr(&target.node);
                let val_type = self.infer_c_type_from_expr(&value.node);
                let v = if target_type == "const char*" && val_type == "GorgetString" {
                    format!("{v}.data")
                } else if target_type == "GorgetString" && val_type == "const char*" {
                    format!("gorget_string_new({v})")
                } else {
                    v
                };
                let str_temps = self.flush_string_temps(emitter);

                // Drop-before-reassign: if the target is a droppable variable,
                // drop the old value before assigning the new one.
                let target_var = if let Expr::Identifier(name) = &target.node {
                    Some(c_mangle::escape_keyword(name))
                } else {
                    None
                };
                let needs_drop = target_var.as_ref()
                    .and_then(|name| self.find_drop_action(name));

                if let Some(drop_action) = needs_drop {
                    // If the RHS moved the target (e.g., s = f(!s)), don't zero it
                    // after giving it a new value
                    if let Some(ref tv) = target_var {
                        self.pending_move_zeros.retain(|v| v != tv);
                    }
                    let tmp = format!("__drop_tmp_{}", self.try_counter);
                    self.try_counter += 1;
                    let c_type = target_type.clone();
                    emitter.emit_line(&format!("{c_type} {tmp} = {v};"));
                    Self::emit_drop_entry(&DropEntry {
                        var_name: target_var.clone().unwrap(),
                        action: drop_action,
                    }, 0, emitter);
                    emitter.emit_line(&format!("{t} = {tmp};"));
                } else {
                    emitter.emit_line(&format!("{t} = {v};"));
                }
                self.flush_move_zeros(emitter);
                Self::emit_string_temp_frees(&str_temps, emitter);
                if self.trace {
                    if let Some(ref ri) = result_info {
                        self.emit_stmt_end_with_result(ri, emitter);
                    } else {
                        self.emit_stmt_end(emitter);
                    }
                }
            }

            Stmt::CompoundAssign { target, op, value } => {
                if self.trace {
                    let vars = self.collect_expr_vars(&[target, value]);
                    self.emit_stmt_start(span, &vars, emitter);
                }
                // String +=: in-place append for GorgetString, leaky concat for const char*
                if *op == BinaryOp::Add {
                    let target_type = self.infer_c_type_from_expr(&target.node);
                    if target_type == "GorgetString" {
                        let t = self.gen_expr(target);
                        let v = self.gen_expr(value);
                        // Coerce rhs to const char* if it's a GorgetString
                        let rhs_type = self.infer_c_type_from_expr(&value.node);
                        let rhs = if rhs_type == "GorgetString" {
                            self.coerce_string_to_str(&v)
                        } else {
                            v
                        };
                        let str_temps = self.flush_string_temps(emitter);
                        emitter.emit_line(&format!("gorget_string_append(&{t}, {rhs});"));
                        Self::emit_string_temp_frees(&str_temps, emitter);
                        if self.trace {
                            self.emit_stmt_end(emitter);
                        }
                        return;
                    }
                    if target_type == "const char*" {
                        let t = self.gen_expr(target);
                        let v = self.gen_expr(value);
                        emitter.emit_line(&format!("{t} = gorget_str_concat({t}, {v});"));
                        if self.trace {
                            self.emit_stmt_end(emitter);
                        }
                        return;
                    }
                }
                // Operator trait dispatch for compound assignment on user types
                let trait_for_op = match op {
                    BinaryOp::Add => Some(("Add", "add")),
                    BinaryOp::Sub => Some(("Sub", "sub")),
                    BinaryOp::Mul => Some(("Mul", "mul")),
                    BinaryOp::Div => Some(("Div", "div")),
                    BinaryOp::Mod => Some(("Rem", "rem")),
                    _ => None,
                };
                if let Some((trait_name, method)) = trait_for_op {
                    if let Some((type_name, trait_type_args)) = self.try_operator_trait_type(target, trait_name) {
                        let t = self.gen_expr(target);
                        let call = self.gen_binary_op_trait_call(trait_name, method, &type_name, &trait_type_args, target, value);
                        let str_temps = self.flush_string_temps(emitter);
                        emitter.emit_line(&format!("{t} = {call};"));
                        Self::emit_string_temp_frees(&str_temps, emitter);
                        if self.trace {
                            self.emit_stmt_end(emitter);
                        }
                        return;
                    }
                }
                let t = self.gen_expr(target);
                let v = self.gen_expr(value);
                if matches!(op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul) && !self.overflow_wrap {
                    let macro_name = match op {
                        BinaryOp::Add => "GORGET_CHECKED_ADD_ASSIGN",
                        BinaryOp::Sub => "GORGET_CHECKED_SUB_ASSIGN",
                        BinaryOp::Mul => "GORGET_CHECKED_MUL_ASSIGN",
                        _ => unreachable!(),
                    };
                    emitter.emit_line(&format!("{macro_name}({t}, {v});"));
                } else {
                    let c_op = compound_op_to_c(*op);
                    emitter.emit_line(&format!("{t} {c_op} {v};"));
                }
                self.flush_move_zeros(emitter);
                if self.trace {
                    self.emit_stmt_end(emitter);
                }
            }

            Stmt::Return(expr) => {
                // Transfer ownership: if returning a variable that has a drop
                // registration, remove it so cleanup doesn't free the value
                // we're about to return.
                if let Some(expr) = expr {
                    // Transfer ownership: walk the expression tree to find all
                    // consumed identifiers (in calls, constructors, variants)
                    let consumed = collect_consumed_identifiers(&expr.node);
                    for name in &consumed {
                        let escaped = c_mangle::escape_keyword(name);
                        if self.escaping_closure_vars.contains(name) {
                            self.remove_droppable(&escaped);
                        }
                        self.remove_droppable(&escaped);
                    }
                    // Inline closure literal in return: heap-allocate it
                    if matches!(&expr.node, Expr::Closure { .. }) {
                        self.closure_heap_alloc = true;
                    }
                    // Handle direct field access returns: zero the source field
                    if let Expr::FieldAccess { object, field } = &expr.node {
                        self.queue_field_move_zero(&object.node, &field.node);
                    }
                }

                if self.has_droppable_entries() {
                    if let Some(expr) = expr {
                        if self.trace {
                            let vars = self.collect_expr_vars(&[expr]);
                            self.emit_stmt_start(span, &vars, emitter);
                        }
                        let e = self.gen_expr(expr);
                        let e = self.coerce_return_value(e, &expr.node);
                        self.closure_heap_alloc = false;
                        let str_temps = self.flush_string_temps(emitter);
                        let ret_type = self.ret_tmp_type(&e);
                        emitter.emit_line(&format!("{ret_type} __ret_tmp = {e};"));
                        self.flush_move_zeros(emitter);
                        Self::emit_string_temp_frees(&str_temps, emitter);
                        if self.trace {
                            self.emit_stmt_end(emitter);
                        }
                        self.emit_cleanup_to(DropScopeKind::Function, emitter);
                        if self.trace {
                            self.emit_trace_return(emitter);
                        }
                        emitter.emit_line("return __ret_tmp;");
                    } else {
                        if self.trace {
                            self.emit_stmt_start(span, &[], emitter);
                            self.emit_stmt_end(emitter);
                        }
                        self.emit_cleanup_to(DropScopeKind::Function, emitter);
                        if self.trace {
                            self.emit_trace_return(emitter);
                        }
                        emitter.emit_line("return;");
                    }
                } else if let Some(expr) = expr {
                    let is_trace = self.trace;
                    if is_trace {
                        let vars = self.collect_expr_vars(&[expr]);
                        self.emit_stmt_start(span, &vars, emitter);
                    }
                    let e = self.gen_expr(expr);
                    let e = self.coerce_return_value(e, &expr.node);
                    self.closure_heap_alloc = false;
                    let str_temps = self.flush_string_temps(emitter);
                    let has_move_zeros = !self.pending_move_zeros.is_empty();
                    if !str_temps.is_empty() || is_trace || has_move_zeros {
                        // Need __ret_tmp to free temps / zero moves before returning
                        let ret_type = self.ret_tmp_type(&e);
                        emitter.emit_line(&format!("{ret_type} __ret_tmp = {e};"));
                        self.flush_move_zeros(emitter);
                        Self::emit_string_temp_frees(&str_temps, emitter);
                        if is_trace {
                            self.emit_stmt_end(emitter);
                            self.emit_trace_return(emitter);
                        }
                        emitter.emit_line("return __ret_tmp;");
                    } else {
                        emitter.emit_line(&format!("return {e};"));
                    }
                } else {
                    if self.trace {
                        self.emit_stmt_start(span, &[], emitter);
                        self.emit_stmt_end(emitter);
                        self.emit_trace_return(emitter);
                    }
                    emitter.emit_line("return;");
                }
            }

            Stmt::Break(_) => {
                if self.has_droppable_entries() {
                    self.emit_cleanup_to(DropScopeKind::Loop, emitter);
                }
                if let Some(flag) = &self.async_break_flag {
                    emitter.emit_line(&format!("{flag} = true;"));
                }
                emitter.emit_line("break;");
            }

            Stmt::Continue => {
                if self.has_droppable_entries() {
                    self.emit_cleanup_to(DropScopeKind::Loop, emitter);
                }
                emitter.emit_line("continue;");
            }

            Stmt::Pass => {
                emitter.emit_line("(void)0;");
            }

            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                // Check if the main condition contains `is` pattern(s) with bindings
                let is_binding_if = Self::has_compound_is_bindings(&condition.node);

                if is_binding_if {
                    self.gen_if_is_binding(condition, then_body, elif_branches, else_body, emitter);
                } else {
                    let cond = self.gen_expr(condition);
                    let cond_temps = self.flush_string_temps(emitter);
                    emitter.emit_line(&format!("if ({cond}) {{"));
                    emitter.indent();
                    if self.trace {
                        let src = c_string_escape(&format!("if {}", self.source_line(condition.span)));
                        let vars = self.collect_expr_vars(&[condition]);
                        self.emit_branch_trace("if", &src, &vars, emitter);
                    }
                    self.push_drop_scope(DropScopeKind::Block);
                    self.gen_block(then_body, emitter);
                    self.pop_drop_scope(emitter);
                    emitter.dedent();

                    // Track wrapper blocks that need closing after elif/else chain.
                    // Elif-is-binding and string-temp elifs wrap in `} else { <temps>; if (...) { body }`.
                    // The inner if is left open so the next elif/else connects to it naturally.
                    // After all branches, we close all wrappers in reverse order.
                    let mut wrapper_temp_frees: Vec<Vec<String>> = Vec::new();

                    for (elif_cond, elif_body) in elif_branches {
                        let elif_is_binding = Self::has_compound_is_bindings(&elif_cond.node);
                        if elif_is_binding {
                            // Wrap in else block so we can declare the scrutinee temp
                            emitter.emit_line("} else {");
                            emitter.indent();
                            let scrut_temps = self.gen_compound_is_condition(elif_cond, "elif", emitter);
                            self.push_drop_scope(DropScopeKind::Block);
                            self.gen_block(elif_body, emitter);
                            self.pop_drop_scope(emitter);
                            emitter.dedent();
                            // Leave inner if open — next elif/else/final `}` closes it
                            wrapper_temp_frees.push(scrut_temps);
                        } else {
                            let ec = self.gen_expr(elif_cond);
                            let elif_temps = self.flush_string_temps_no_emit();
                            if elif_temps.is_empty() {
                                emitter.emit_line(&format!("}} else if ({ec}) {{"));
                                emitter.indent();
                                if self.trace {
                                    let src = c_string_escape(&format!("elif {}", self.source_line(elif_cond.span)));
                                    let vars = self.collect_expr_vars(&[elif_cond]);
                                    self.emit_branch_trace("elif", &src, &vars, emitter);
                                }
                                self.push_drop_scope(DropScopeKind::Block);
                                self.gen_block(elif_body, emitter);
                                self.pop_drop_scope(emitter);
                                emitter.dedent();
                            } else {
                                // String-temp elif: wrap in else block for temp declarations
                                emitter.emit_line("} else {");
                                emitter.indent();
                                for (name, expr) in &elif_temps {
                                    emitter.emit_line(&format!("GorgetString {name} = {expr};"));
                                }
                                emitter.emit_line(&format!("if ({ec}) {{"));
                                emitter.indent();
                                if self.trace {
                                    let src = c_string_escape(&format!("elif {}", self.source_line(elif_cond.span)));
                                    let vars = self.collect_expr_vars(&[elif_cond]);
                                    self.emit_branch_trace("elif", &src, &vars, emitter);
                                }
                                self.push_drop_scope(DropScopeKind::Block);
                                self.gen_block(elif_body, emitter);
                                self.pop_drop_scope(emitter);
                                emitter.dedent();
                                // Leave inner if open — next elif/else/final `}` closes it
                                let names: Vec<String> = elif_temps.iter().map(|(n, _)| n.clone()).collect();
                                wrapper_temp_frees.push(names);
                            }
                        }
                    }

                    if let Some(else_body) = else_body {
                        emitter.emit_line("} else {");
                        emitter.indent();
                        if self.trace {
                            self.emit_branch_trace("else", "else", &[], emitter);
                        }
                        self.push_drop_scope(DropScopeKind::Block);
                        self.gen_block(else_body, emitter);
                        self.pop_drop_scope(emitter);
                        emitter.dedent();
                    }

                    emitter.emit_line("}");
                    // Close all wrapper blocks in reverse order
                    for temps in wrapper_temp_frees.iter().rev() {
                        Self::emit_string_temp_frees(temps, emitter);
                        emitter.dedent();
                        emitter.emit_line("}");
                    }
                    Self::emit_string_temp_frees(&cond_temps, emitter);
                }
            }

            Stmt::While {
                condition,
                body,
                else_body,
            } => {
                // Check if condition contains `is` pattern(s) with bindings
                let is_binding_while = Self::has_compound_is_bindings(&condition.node);

                if is_binding_while {
                    self.gen_while_is_binding(condition, body, else_body.as_ref(), emitter);
                } else {
                    let cond = self.gen_expr(condition);
                    let trace_iter = if self.trace {
                        let t = emitter.fresh_temp();
                        emitter.emit_line(&format!("int64_t {t} = 0;"));
                        Some(t)
                    } else {
                        None
                    };
                    if let Some(else_block) = else_body {
                        let flag = emitter.fresh_temp();
                        emitter.emit_line(&format!("bool {flag} = false;"));
                        emitter.emit_line(&format!("while ({cond}) {{"));
                        emitter.indent();
                        self.push_drop_scope(DropScopeKind::Loop);
                        if let Some(ref ti) = trace_iter {
                            self.emit_while_iter_trace(ti, emitter);
                        }
                        self.gen_block_with_break_flag(body, &flag, emitter);
                        self.pop_drop_scope(emitter);
                        emitter.dedent();
                        emitter.emit_line("}");
                        emitter.emit_line(&format!("if (!{flag}) {{"));
                        emitter.indent();
                        self.gen_block(else_block, emitter);
                        emitter.dedent();
                        emitter.emit_line("}");
                    } else {
                        emitter.emit_line(&format!("while ({cond}) {{"));
                        emitter.indent();
                        self.push_drop_scope(DropScopeKind::Loop);
                        if let Some(ref ti) = trace_iter {
                            self.emit_while_iter_trace(ti, emitter);
                        }
                        self.gen_block(body, emitter);
                        self.pop_drop_scope(emitter);
                        emitter.dedent();
                        emitter.emit_line("}");
                    }
                }
            }

            Stmt::Loop { body } => {
                emitter.emit_line("while (1) {");
                emitter.indent();
                self.push_drop_scope(DropScopeKind::Loop);
                self.gen_block(body, emitter);
                self.pop_drop_scope(emitter);
                emitter.dedent();
                emitter.emit_line("}");
            }

            Stmt::For {
                pattern,
                iterable,
                body,
                else_body,
                ..
            } => {
                self.gen_for_loop(pattern, iterable, body, else_body, emitter);
            }

            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.gen_match_stmt(scrutinee, arms, else_arm, emitter);
            }

            Stmt::Throw(expr) => {
                let e = self.gen_expr(expr);
                let str_temps = self.flush_string_temps(emitter);
                // str maps to const char* in all cases — pass directly
                emitter.emit_line(&format!("GORGET_THROW({e}, 1);"));
                Self::emit_string_temp_frees(&str_temps, emitter);
            }

            Stmt::With { bindings, body } => {
                emitter.emit_line("{");
                emitter.indent();
                for binding in bindings {
                    let e = self.gen_expr(&binding.expr);
                    let name = c_mangle::escape_keyword(&binding.name.node);
                    emitter.emit_line(&format!("/* with */ void* {name} = (void*)({e});"));
                }
                self.push_drop_scope(DropScopeKind::Block);
                self.gen_block(body, emitter);
                self.pop_drop_scope(emitter);
                emitter.dedent();
                emitter.emit_line("}");
            }

            Stmt::Unsafe { body } => {
                emitter.emit_line("/* unsafe */ {");
                emitter.indent();
                self.push_drop_scope(DropScopeKind::Block);
                self.gen_block(body, emitter);
                self.pop_drop_scope(emitter);
                emitter.dedent();
                emitter.emit_line("}");
            }

            Stmt::Assert { condition, message } => {
                if !self.strip_asserts {
                    if self.trace {
                        emitter.emit_line("{");
                        emitter.indent();
                        let vars = self.collect_expr_vars(&[condition]);
                        self.emit_stmt_start(span, &vars, emitter);
                        let cond = self.gen_expr(condition);
                        let msg = match message {
                            Some(m) => self.gen_expr(m),
                            None => self.gen_assert_diagnostic(condition)
                                .unwrap_or_else(|| "\"assertion failed\"".to_string()),
                        };
                        let str_temps = self.flush_string_temps(emitter);
                        emitter.emit_line(&format!("_Bool __assert_cond = ({cond});"));
                        Self::emit_string_temp_frees(&str_temps, emitter);
                        self.emit_stmt_end(emitter);
                        emitter.emit_line(&format!("if (!__assert_cond) gorget_panic({msg});"));
                        emitter.dedent();
                        emitter.emit_line("}");
                    } else {
                        let cond = self.gen_expr(condition);
                        let msg = match message {
                            Some(m) => self.gen_expr(m),
                            None => self.gen_assert_diagnostic(condition)
                                .unwrap_or_else(|| "\"assertion failed\"".to_string()),
                        };
                        let str_temps = self.flush_string_temps(emitter);
                        emitter.emit_line(&format!("if (!({cond})) gorget_panic({msg});"));
                        Self::emit_string_temp_frees(&str_temps, emitter);
                    }
                }
            }

            Stmt::Item(_) => {
                // Nested items handled during top-level pass
            }
        }
    }

    /// Map comparison operators to their textual representation.
    fn comparison_op_str(op: &BinaryOp) -> Option<&'static str> {
        match op {
            BinaryOp::Eq => Some("=="),
            BinaryOp::Neq => Some("!="),
            BinaryOp::Lt => Some("<"),
            BinaryOp::Gt => Some(">"),
            BinaryOp::LtEq => Some("<="),
            BinaryOp::GtEq => Some(">="),
            _ => None,
        }
    }

    /// Generate a rich diagnostic message for assert failures on comparisons.
    /// Returns `Some(gorget_format(...))` if the condition is a comparison with
    /// formattable operands, or `None` to fall back to the default message.
    fn gen_assert_diagnostic(&mut self, condition: &Spanned<Expr>) -> Option<String> {
        let (left, op, right) = match &condition.node {
            Expr::BinaryOp { left, op, right } => (left, op, right),
            _ => return None,
        };

        let op_str = Self::comparison_op_str(op)?;

        let left_tid = self.resolve_expr_type_id(left)?;
        let right_tid = self.resolve_expr_type_id(right)?;

        let left_c = self.gen_expr(left);
        let right_c = self.gen_expr(right);

        let (left_fmt, left_arg) = self.format_for_type_id(left_tid, &left_c);
        let (right_fmt, right_arg) = self.format_for_type_id(right_tid, &right_c);

        Some(format!(
            "gorget_format(\"assertion failed: left {op_str} right\\n  left:  {left_fmt}\\n  right: {right_fmt}\", {left_arg}, {right_arg})"
        ))
    }

    /// Generate a variable declaration.
    fn gen_var_decl(
        &mut self,
        is_const: bool,
        type_: &Spanned<Type>,
        pattern: &Spanned<Pattern>,
        value: &Spanned<Expr>,
        span: Span,
        emitter: &mut CEmitter,
    ) {
        // Reset deep-clone flag — set by .get() codegen if element was deep-cloned
        self.deep_cloned_expr = false;

        match &pattern.node {
            Pattern::Binding(name) => {
                let escaped = c_mangle::escape_keyword(name);
                let const_prefix = if is_const { "const " } else { "" };

                let result_info = if self.trace {
                    let vars = self.collect_expr_vars(&[value]);
                    self.emit_stmt_start(span, &vars, emitter);
                    self.lookup_var_trace_info(name)
                } else {
                    None
                };

                // Special handling for Box[Callable[sig]] construction
                if let Some((kind_prefix, fn_ret, fn_params, fn_ownerships)) = Self::extract_box_callable_type(type_) {
                    let ret_c = super::c_types::ast_type_to_c(&fn_ret.node, self.scopes);
                    let param_c: Vec<String> = fn_params.iter().enumerate()
                        .map(|(i, p)| {
                            let base = super::c_types::ast_type_to_c(&p.node, self.scopes);
                            let ownership = fn_ownerships.get(i).copied().unwrap_or(Ownership::Borrow);
                            if matches!(ownership, Ownership::MutableBorrow) {
                                format!("{base}*")
                            } else {
                                base
                            }
                        })
                        .collect();
                    let sig_name = super::c_item::callable_sig_name(kind_prefix, &param_c, &ret_c);
                    let traitobj_type = format!("{sig_name}__TraitObj");

                    // Unwrap Box.new(closure) or accept bare closure (auto-boxing)
                    let closure_expr = if let Expr::Call { callee, args: box_args, .. } = &value.node {
                        // Call with Path callee: Box.new(x) parsed as Call { Path(["Box","new"]), [x] }
                        if let Expr::Path { segments } = &callee.node {
                            if segments.len() == 2 && segments[0].node == "Box" && segments[1].node == "new" {
                                box_args.first().map(|a| &a.node.value)
                            } else { None }
                        // Call with Identifier callee: Box(x)
                        } else if let Expr::Identifier(n) = &callee.node {
                            if n == "Box" { box_args.first().map(|a| &a.node.value) } else { None }
                        } else { None }
                    } else if let Expr::MethodCall { receiver, method, args: mc_args, .. } = &value.node {
                        // MethodCall: Box.new(x) parsed as MethodCall { Identifier("Box"), "new", [x] }
                        if let Expr::Identifier(n) = &receiver.node {
                            if n == "Box" && method.node == "new" {
                                mc_args.first().map(|a| &a.node.value)
                            } else { None }
                        } else { None }
                    } else {
                        Some(value) // Auto-boxing: bare closure
                    };

                    if let Some(closure_val) = closure_expr {
                        // Force heap-alloc for the closure so the env outlives the stack
                        self.closure_heap_alloc = true;
                        let val = self.gen_expr(closure_val);
                        self.closure_heap_alloc = false;

                        // Get the last-pushed lifted closure to determine struct name
                        let closure_info = self.lifted_closures.last().map(|lc| {
                            (lc.struct_name.clone(), lc.captures.is_empty())
                        });
                        if let Some((struct_name, is_non_capturing)) = closure_info {
                            let vtable_inst = format!("{sig_name}__{struct_name}__vtable");
                            // Register the sig for vtable emission
                            let callable_kind = match kind_prefix {
                                "Callable" => super::CallableKind::Callable,
                                "MutCallable" => super::CallableKind::MutCallable,
                                _ => super::CallableKind::ConsumeCallable,
                            };
                            let sig = (callable_kind, param_c.clone(), ret_c.clone());
                            if !self.fn_trait_sigs.contains(&sig) {
                                self.fn_trait_sigs.push(sig);
                            }

                            if is_non_capturing {
                                // Non-capturing: env=NULL
                                emitter.emit_line(&format!(
                                    "{const_prefix}{traitobj_type} {escaped} = {{ .data = NULL, .vtable = &{vtable_inst} }};"
                                ));
                            } else {
                                // Capturing: the GorgetClosure expression has a heap-alloc env
                                // Extract env pointer from the GorgetClosure
                                let tmp = format!("__box_closure_{escaped}");
                                emitter.emit_line(&format!("GorgetClosure {tmp} = {val};"));
                                emitter.emit_line(&format!(
                                    "{const_prefix}{traitobj_type} {escaped} = {{ .data = {tmp}.env, .vtable = &{vtable_inst} }};"
                                ));
                            }

                            // Register in closure_var_info for vtable dispatch
                            self.closure_var_info.insert(escaped.clone(), super::ClosureVarInfo::TraitObject {
                                param_c_types: param_c,
                                return_c_type: ret_c,
                                kind: callable_kind,
                            });
                        } else {
                            // Fallback: shouldn't happen, but emit generic code
                            emitter.emit_line(&format!("{const_prefix}{traitobj_type} {escaped} = {val};"));
                        }

                        if self.trace {
                            if let Some(ref ri) = result_info {
                                self.emit_stmt_end_with_result(ri, emitter);
                            } else {
                                self.emit_stmt_end(emitter);
                            }
                        }
                        return;
                    }
                }

                // Special handling for trait object construction:
                // Box[dynamic Trait] x = Box.new(ConcreteType(...))
                if let Some((trait_name, concrete_type, inner_expr)) =
                    self.extract_trait_object_construction(type_, value)
                {
                    let trait_obj_type = c_mangle::mangle_trait_obj(&trait_name);
                    let vtable_instance = c_mangle::mangle_vtable_instance(&trait_name, &concrete_type);
                    let inner_val = self.gen_expr(inner_expr);

                    emitter.emit_line(&format!(
                        "{concrete_type}* __box_{escaped} = ({concrete_type}*)malloc(sizeof({concrete_type}));"
                    ));
                    emitter.emit_line(&format!("*__box_{escaped} = {inner_val};"));
                    emitter.emit_line(&format!(
                        "{const_prefix}{trait_obj_type} {escaped} = {{ .data = (void*)__box_{escaped}, .vtable = &{vtable_instance} }};"
                    ));
                    if self.trace {
                        if let Some(ref ri) = result_info {
                            self.emit_stmt_end_with_result(ri, emitter);
                        } else {
                            self.emit_stmt_end(emitter);
                        }
                    }
                    return;
                }

                // Special handling for array literals: emit as C array declarations.
                // But NOT when the declared type is a collection type like Vector[T].
                if let Expr::ArrayLiteral(elements) = &value.node {
                    let is_collection_type = matches!(&type_.node,
                        Type::Named { name, generic_args } if !generic_args.is_empty()
                            && matches!(name.node.as_str(), "Vector" | "List" | "Array" | "Set" | "Dict" | "HashMap" | "Map")
                    );
                    let is_auto = matches!(&type_.node, Type::Inferred);
                    if !is_collection_type && !is_auto {
                        // Explicit array type (e.g. int[3] arr = [1, 2, 3]) → C array
                        let elem_type = self.type_to_c(&type_.node);
                        let elems: Vec<String> = elements.iter().map(|e| self.gen_expr(e)).collect();
                        emitter.emit_line(&format!(
                            "{const_prefix}{elem_type} {escaped}[] = {{{}}};",
                            elems.join(", ")
                        ));
                        if self.trace {
                            if let Some(ref ri) = result_info {
                                self.emit_stmt_end_with_result(ri, emitter);
                            } else {
                                self.emit_stmt_end(emitter);
                            }
                        }
                        return;
                    }
                    // Collection type or auto with array literal: create a GorgetArray and push elements
                    let elem_type = if is_auto {
                        if let Some(first) = elements.first() {
                            self.infer_c_type_from_expr(&first.node)
                        } else {
                            "int64_t".to_string()
                        }
                    } else if let Type::Named { generic_args, .. } = &type_.node {
                        if let Some(first_arg) = generic_args.first() {
                            self.type_to_c(&first_arg.node)
                        } else {
                            "int64_t".to_string()
                        }
                    } else {
                        "int64_t".to_string()
                    };
                    emitter.emit_line(&format!(
                        "{const_prefix}GorgetArray {escaped} = gorget_array_new(sizeof({elem_type}));"
                    ));
                    for e in elements {
                        let val = self.gen_expr(e);
                        emitter.emit_line(&format!(
                            "{{ {elem_type} __tmp = {val}; gorget_array_push(&{escaped}, &__tmp); }}"
                        ));
                    }
                    if is_auto {
                        self.vector_vars.insert(escaped.clone());
                    }
                    if self.trace {
                        if let Some(ref ri) = result_info {
                            self.emit_stmt_end_with_result(ri, emitter);
                        } else {
                            self.emit_stmt_end(emitter);
                        }
                    }
                    return;
                }

                // Special handling for tuple literals: emit with named typedef
                if let Expr::TupleLiteral(elements) = &value.node {
                    let elems: Vec<String> = elements.iter().map(|e| self.gen_expr(e)).collect();
                    let c_field_types: Vec<String> = elements
                        .iter()
                        .map(|elem| self.infer_c_type_from_expr(&elem.node))
                        .collect();
                    let tuple_name = self.register_tuple_typedef(&c_field_types);
                    emitter.emit_line(&format!(
                        "{const_prefix}{tuple_name} {escaped} = ({tuple_name}){{{}}};",
                        elems.join(", ")
                    ));
                    if self.trace {
                        if let Some(ref ri) = result_info {
                            self.emit_stmt_end_with_result(ri, emitter);
                        } else {
                            self.emit_stmt_end(emitter);
                        }
                    }
                    return;
                }

                let c_type = self.resolve_decl_type(type_, value, Some(name));
                let is_escaping_closure = c_type == "GorgetClosure"
                    && self.escaping_closure_vars.contains(name);
                if c_type == "GorgetClosure" {
                    self.closure_vars.insert(escaped.clone());
                    // For Callable/MutCallable/ConsumeCallable-typed VarDecl, extract signature
                    if let Type::Named { name: fn_name, generic_args } = &type_.node {
                        let callable_kind = match fn_name.node.as_str() {
                            "Callable" => Some(super::CallableKind::Callable),
                            "MutCallable" => Some(super::CallableKind::MutCallable),
                            "ConsumeCallable" => Some(super::CallableKind::ConsumeCallable),
                            _ => None,
                        };
                        if let Some(_kind) = callable_kind {
                            if generic_args.len() == 1 {
                                if let Type::Function { return_type: fn_ret, params: fn_params, param_ownerships } = &generic_args[0].node {
                                    let ret_c = super::c_types::ast_type_to_c(&fn_ret.node, self.scopes);
                                    let param_c: Vec<String> = fn_params.iter().enumerate()
                                        .map(|(i, p)| {
                                            let base = super::c_types::ast_type_to_c(&p.node, self.scopes);
                                            let ownership = param_ownerships.get(i).copied().unwrap_or(Ownership::Borrow);
                                            if matches!(ownership, Ownership::MutableBorrow) {
                                                format!("{base}*")
                                            } else {
                                                base
                                            }
                                        })
                                        .collect();
                                    self.fn_type_signatures.insert(escaped.clone(), (param_c, ret_c, param_ownerships.clone()));
                                }
                            }
                        }
                    }
                }
                // Set heap-alloc flag for escaping closures
                if is_escaping_closure {
                    self.closure_heap_alloc = true;
                }
                // Set type hint for unit variant constructors like None()
                let prev_hint = self.decl_type_hint.clone();
                self.decl_type_hint = Some(type_.node.clone());
                let val = self.gen_expr(value);
                self.decl_type_hint = prev_hint;
                self.closure_heap_alloc = false;
                // Coerce non-capturing closure to GorgetClosure for Callable-typed vars only.
                let is_fn_trait_decl = matches!(&type_.node, Type::Named { name, generic_args }
                    if matches!(name.node.as_str(), "Callable" | "MutCallable" | "ConsumeCallable")
                    && !generic_args.is_empty());
                let val = if is_fn_trait_decl && !val.starts_with("(GorgetClosure)") {
                    let val_type = self.infer_c_type_from_expr(&value.node);
                    if val_type != "GorgetClosure" {
                        // Use the _fn adapter for non-capturing closure functions
                        let fn_ptr = if val.starts_with("__gorget_closure_") {
                            format!("{val}_fn")
                        } else {
                            val
                        };
                        format!("(GorgetClosure){{.fn_ptr = (void*){fn_ptr}, .env = NULL}}")
                    } else {
                        val
                    }
                } else {
                    val
                };
                // Coerce between String and str at declaration
                let val = if c_type == "const char*" {
                    let val_type = self.infer_c_type_from_expr(&value.node);
                    if val_type == "GorgetString" {
                        format!("{val}.data")
                    } else {
                        val
                    }
                } else if c_type == "GorgetString" {
                    let val_type = self.infer_c_type_from_expr(&value.node);
                    if val_type == "const char*" {
                        format!("gorget_string_new({val})")
                    } else {
                        val
                    }
                } else {
                    val
                };
                let str_temps = self.flush_string_temps(emitter);
                let decl = c_types::c_declare(&c_type, &escaped);
                emitter.emit_line(&format!("{const_prefix}{decl} = {val};"));
                self.flush_move_zeros(emitter);
                Self::emit_string_temp_frees(&str_temps, emitter);

                // Track explicitly-typed Vector variables so is_vector_expr recognizes them
                if c_type == "GorgetArray" {
                    self.vector_vars.insert(escaped.clone());
                }

                if self.trace {
                    if let Some(ref ri) = result_info {
                        self.emit_stmt_end_with_result(ri, emitter);
                    } else {
                        self.emit_stmt_end(emitter);
                    }
                }

                // Register droppable variable for RAII cleanup
                if is_escaping_closure {
                    self.register_droppable(&escaped, DropAction::ClosureEnvFree);
                } else if c_type == "GorgetString" {
                    // Auto-inferred String type needs drop registration
                    self.register_droppable(&escaped, DropAction::StringFree);
                    if self.in_test_body {
                        emitter.emit_line(&format!(
                            "__gorget_cleanup_push((__gorget_cleanup_fn)gorget_string_free, (void*)&{escaped});"
                        ));
                    }
                } else {
                    // For collection-related drops, only register when the init
                    // expression is a known constructor (to avoid aliased-buffer
                    // double-frees from copies/field-access/function-returns).
                    // Non-collection drops (Box, File, user Drop, etc.) always register.
                    //
                    // When the type is Inferred (auto), drop_action_for_type returns None.
                    // Fall back to looking up the C type as a struct name for deep-cloned values.
                    let action = self.drop_action_for_type(&type_.node).or_else(|| {
                        if self.deep_cloned_expr {
                            // Auto-typed deep-cloned value: look up struct by C type name
                            self.drop_action_for_struct_name(&c_type)
                        } else {
                            None
                        }
                    });
                    if let Some(action) = action {
                        if !action.involves_collection() {
                            // Non-collection: always register full drops
                            self.register_droppable(&escaped, action.clone());
                            if self.in_test_body {
                                Self::emit_test_cleanup_for_action(&escaped, &action, emitter);
                            }
                        } else if Self::is_owning_collection_init(&value.node) {
                            // Constructor-initialized: full drops (elements uniquely owned)
                            self.register_droppable(&escaped, action.clone());
                            if self.in_test_body {
                                Self::emit_test_cleanup_for_action(&escaped, &action, emitter);
                            }
                        } else if Self::is_fresh_allocation_source(&value.node) {
                            // Function/method return: buffer-only (elements may be aliased)
                            let buf_action = action.buffer_only();
                            self.register_droppable(&escaped, buf_action.clone());
                            if self.in_test_body {
                                Self::emit_test_cleanup_for_action(&escaped, &buf_action, emitter);
                            }
                        } else if self.try_queue_field_move_zero_for_value(&value.node) {
                            // Field access where source field was zeroed: variable is sole owner
                            self.register_droppable(&escaped, action.clone());
                            // Flush immediately — the initial flush_move_zeros() ran before
                            // this check, so queued zeros must be emitted now to avoid
                            // deferral into a conditional branch.
                            self.flush_move_zeros(emitter);
                            if self.in_test_body {
                                Self::emit_test_cleanup_for_action(&escaped, &action, emitter);
                            }
                        } else if self.deep_cloned_expr {
                            // .get() deep-cloned: variable owns independent buffer copies.
                            // Function call args now have field-move-zeroing, and VarDecl
                            // field extraction already has field-move-zeroing, so full
                            // drops (including StructDrop with field drops) are safe.
                            self.deep_cloned_expr = false;
                            self.register_droppable(&escaped, action.clone());
                            if self.in_test_body {
                                Self::emit_test_cleanup_for_action(&escaped, &action, emitter);
                            }
                        }
                        // Otherwise: variable copy/field access — no drop (aliased buffer)
                    }
                }
            }
            Pattern::Wildcard => {
                if self.trace {
                    let vars = self.collect_expr_vars(&[value]);
                    self.emit_stmt_start(span, &vars, emitter);
                }
                let val = self.gen_expr(value);
                emitter.emit_line(&format!("(void){val};"));
                self.flush_move_zeros(emitter);
                if self.trace {
                    self.emit_stmt_end(emitter);
                }
            }
            _ => {
                if self.trace {
                    let vars = self.collect_expr_vars(&[value]);
                    self.emit_stmt_start(span, &vars, emitter);
                }
                let c_type = self.resolve_decl_type(type_, value, None);
                let val = self.gen_expr(value);
                let tmp = emitter.fresh_temp();
                let decl = c_types::c_declare(&c_type, &tmp);
                emitter.emit_line(&format!("{decl} = {val};"));
                if !matches!(type_.node, Type::Inferred) {
                    self.emit_typed_pattern_bindings(&pattern.node, &tmp, &type_.node, emitter);
                } else {
                    // For auto: try to resolve the value's TypeId to emit typed bindings
                    let resolved_tuple_field_types = self.resolve_expr_type_id(value)
                        .and_then(|tid| {
                            if let crate::semantic::types::ResolvedType::Tuple(field_tids) = self.types.get(tid).clone() {
                                Some(field_tids)
                            } else {
                                None
                            }
                        });
                    if let (Some(field_tids), Pattern::Tuple(elements)) = (&resolved_tuple_field_types, &pattern.node) {
                        for (i, elem) in elements.iter().enumerate() {
                            let field_access = format!("{tmp}._{i}");
                            if let Some(field_tid) = field_tids.get(i) {
                                let field_c_type = c_types::type_id_to_c(*field_tid, self.types, self.scopes);
                                if let Pattern::Binding(name) = &elem.node {
                                    let escaped = c_mangle::escape_keyword(name);
                                    let field_decl = c_types::c_declare(&field_c_type, &escaped);
                                    emitter.emit_line(&format!("{field_decl} = {field_access};"));
                                    self.pattern_var_types.insert(name.clone(), *field_tid);
                                } else {
                                    self.emit_pattern_bindings(&elem.node, &field_access, emitter);
                                }
                            } else {
                                self.emit_pattern_bindings(&elem.node, &field_access, emitter);
                            }
                        }
                    } else {
                        self.emit_pattern_bindings(&pattern.node, &tmp, emitter);
                    }
                }
                self.flush_move_zeros(emitter);
                if self.trace {
                    self.emit_stmt_end(emitter);
                }
            }
        }
    }

    /// Resolve the C type for a declaration, handling `auto` (Inferred).
    /// Also registers generic instantiations when a user-defined generic type is used.
    fn resolve_decl_type(
        &mut self,
        type_: &Spanned<Type>,
        value: &Spanned<Expr>,
        var_name: Option<&str>,
    ) -> String {
        match &type_.node {
            Type::Inferred => {
                // Special-case capturing closures → GorgetClosure
                if let Expr::Closure { params, body, .. } = &value.node {
                    let bound: std::collections::HashSet<&str> =
                        params.iter().map(|p| p.node.name.node.as_str()).collect();
                    let captures = self.collect_free_vars(body, &bound);
                    if !captures.is_empty() {
                        return "GorgetClosure".to_string();
                    }
                }
                // First, check if the semantic analysis stored a resolved type
                if let Some(name) = var_name {
                    if let Some(def_id) = self.scoped_lookup(name) {
                        let def = self.scopes.get_def(def_id);
                        if let Some(type_id) = def.type_id {
                            let c_type = c_types::type_id_to_c(type_id, self.types, self.scopes);
                            // Function types from calls that return closures should
                            // be stored as GorgetClosure (the actual C return type).
                            if c_type.contains("(*)") && matches!(&value.node, Expr::Call { .. }) {
                                return "GorgetClosure".to_string();
                            }
                            return c_type;
                        }
                    }
                }
                // Fall back to expression-based inference
                self.infer_c_type_from_expr(&value.node)
            }
            _ => self.type_to_c(&type_.node),
        }
    }

    /// Best-effort C type inference from a value expression.
    pub(super) fn infer_c_type_from_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::IntLiteral(_) => "int64_t".to_string(),
            Expr::FloatLiteral(_) => "double".to_string(),
            Expr::BoolLiteral(_) => "bool".to_string(),
            Expr::CharLiteral(_) => "char".to_string(),
            Expr::StringLiteral(s) => {
                use crate::lexer::token::StringSegment;
                if s.segments.iter().any(|seg| matches!(seg, StringSegment::Interpolation(_))) {
                    "GorgetString".to_string()
                } else {
                    "const char*".to_string()
                }
            }
            Expr::BinaryOp { op, left, right, .. } => {
                use crate::parser::ast::BinaryOp;
                match op {
                    BinaryOp::Eq
                    | BinaryOp::Neq
                    | BinaryOp::Lt
                    | BinaryOp::Gt
                    | BinaryOp::LtEq
                    | BinaryOp::GtEq
                    | BinaryOp::And
                    | BinaryOp::Or
                    | BinaryOp::In => "bool".to_string(),
                    BinaryOp::Add => {
                        let left_type = self.infer_c_type_from_expr(&left.node);
                        if left_type == "const char*" || left_type == "GorgetString" {
                            return "GorgetString".to_string();
                        }
                        let right_type = self.infer_c_type_from_expr(&right.node);
                        if right_type == "const char*" || right_type == "GorgetString" {
                            return "GorgetString".to_string();
                        }
                        left_type
                    }
                    _ => self.infer_c_type_from_expr(&left.node),
                }
            }
            Expr::UnaryOp { operand, .. } => self.infer_c_type_from_expr(&operand.node),
            Expr::Call { callee, .. } => {
                // Try to look up the return type of the function
                if let Expr::Identifier(name) = &callee.node {
                    // Builtin return types (compiler builtins only; stdlib uses function_info)
                    match name.as_str() {
                        "format" => return "GorgetString".to_string(),
                        "Channel" => return "GorgetChannel*".to_string(),
                        _ => {}
                    }
                    if let Some(def_id) = self.scoped_lookup(name) {
                        if let Some(func_info) = self.function_info.get(&def_id) {
                            if let Some(ret_type_id) = func_info.return_type_id {
                                return self.type_id_to_c_substituted(ret_type_id);
                            }
                        }
                    }
                }
                "int64_t".to_string()
            }
            Expr::Identifier(name) => {
                if let Some(def_id) = self.scoped_lookup(name) {
                    let def = self.scopes.get_def(def_id);
                    if let Some(type_id) = def.type_id {
                        return self.type_id_to_c_substituted(type_id);
                    }
                }
                "int64_t".to_string()
            }
            Expr::FieldAccess { object, field } => {
                // Check via type_id for built-in struct fields
                if let Some(obj_tid) = self.resolve_expr_type_id(object) {
                    if obj_tid == self.types.owned_string_id {
                        return match field.node.as_str() {
                            "data" => "const char*".to_string(),
                            "len" | "cap" => "size_t".to_string(),
                            _ => "int64_t".to_string(),
                        };
                    }
                }
                let obj_type = self.infer_receiver_type(object);
                if obj_type != "Unknown" {
                    let key = (obj_type, field.node.clone());
                    if let Some(field_type) = self.field_type_names.get(&key) {
                        return super::c_types::ast_type_to_c(field_type, self.scopes);
                    }
                }
                "int64_t".to_string()
            }
            Expr::StructLiteral { name, .. } => name.node.clone(),
            Expr::DictLiteral(pairs) => {
                if pairs.is_empty() {
                    if let Some(crate::parser::ast::Type::Named { name, generic_args }) = self.decl_type_hint.as_ref() {
                        if matches!(name.node.as_str(), "Dict" | "HashMap") && generic_args.len() >= 2 {
                            let key_c = self.type_to_c(&generic_args[0].node);
                            let val_c = self.type_to_c(&generic_args[1].node);
                            let base = if name.node == "Dict" { "GorgetDict" } else { "GorgetMap" };
                            return c_mangle::mangle_generic(base, &[key_c, val_c]);
                        }
                    }
                    c_mangle::mangle_generic("GorgetDict", &["int64_t".to_string(), "int64_t".to_string()])
                } else {
                    let key_c = self.infer_c_type_from_expr(&pairs[0].0.node);
                    let val_c = self.infer_c_type_from_expr(&pairs[0].1.node);
                    c_mangle::mangle_generic("GorgetDict", &[key_c, val_c])
                }
            }
            Expr::TupleLiteral(elements) => {
                let c_field_types: Vec<String> = elements
                    .iter()
                    .map(|elem| self.infer_c_type_from_expr(&elem.node))
                    .collect();
                super::c_mangle::mangle_tuple(&c_field_types)
            }
            Expr::MethodCall {
                receiver, method, ..
            } => {
                let recv_c_type = self.infer_receiver_c_type(receiver);
                if let Some(tid) = recv_c_type
                    .as_deref()
                    .and_then(|rt| self.builtin_method_return_type(rt, &method.node))
                {
                    return self.type_id_to_c_substituted(tid);
                }
                let type_name = self.infer_receiver_type(receiver);
                for impl_info in &self.traits.impls {
                    if impl_info.self_type_name == type_name {
                        if let Some((_def_id, sig)) = impl_info.methods.get(method.node.as_str())
                        {
                            return self.type_id_to_c_substituted(sig.return_type);
                        }
                    }
                }
                "int64_t".to_string()
            }
            Expr::Block(block) | Expr::Do { body: block } => {
                // Infer from the last statement of the block
                if let Some(last) = block.stmts.last() {
                    if let crate::parser::ast::Stmt::Expr(e) = &last.node {
                        return self.infer_c_type_from_expr(&e.node);
                    }
                }
                "void".to_string()
            }
            Expr::Spawn { expr: inner } => {
                let future_type = self.infer_c_type_from_expr(&inner.node);
                // Future__T → Task__T
                if let Some(suffix) = future_type.strip_prefix("Future__") {
                    format!("Task__{suffix}")
                } else {
                    future_type.replacen("Future", "Task", 1)
                }
            }
            Expr::Await { expr: inner } => {
                let inner_type = self.infer_c_type_from_expr(&inner.node);
                // Future__T → T, Task__T → T
                if let Some(suffix) = inner_type.strip_prefix("Future__")
                    .or_else(|| inner_type.strip_prefix("Task__"))
                {
                    suffix.to_string()
                } else {
                    inner_type
                }
            }
            _ => "int64_t".to_string(),
        }
    }

    /// Detect trait object construction pattern: `Box[Trait] x = Box.new(ConcreteValue)`.
    /// When the type annotation is `Box[T]` and `T` is a trait, this extracts the
    /// trait name, concrete type name, and inner expression for trait object codegen.
    fn extract_trait_object_construction<'b>(
        &mut self,
        type_: &Spanned<Type>,
        value: &'b Spanned<Expr>,
    ) -> Option<(String, String, &'b Spanned<Expr>)> {
        // Check if type is Box[TraitName] where TraitName is a trait
        let trait_name = match &type_.node {
            Type::Named { name, generic_args }
                if name.node == "Box" && generic_args.len() == 1 =>
            {
                if let Type::Named {
                    name: inner_name,
                    generic_args: inner_args,
                } = &generic_args[0].node
                {
                    if inner_args.is_empty() {
                        let def_id = self.scopes.lookup(&inner_name.node)?;
                        let def = self.scopes.get_def(def_id);
                        if def.kind == crate::semantic::scope::DefKind::Trait {
                            Some(inner_name.node.clone())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }?;

        // Extract inner expression from Box.new(expr) or Box(expr)
        let inner_expr = match &value.node {
            Expr::MethodCall {
                receiver,
                method,
                args,
                ..
            } => {
                if let Expr::Identifier(name) = &receiver.node {
                    if name == "Box" && method.node == "new" && args.len() == 1 {
                        Some(&args[0].node.value)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Expr::Call { callee, args, .. } => {
                if let Expr::Identifier(name) = &callee.node {
                    if name == "Box" && args.len() == 1 {
                        Some(&args[0].node.value)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }?;

        // Infer the concrete type from the inner expression
        let concrete_type = match &inner_expr.node {
            Expr::Call { callee, .. } => {
                if let Expr::Identifier(name) = &callee.node {
                    Some(name.clone())
                } else {
                    None
                }
            }
            Expr::StructLiteral { name, .. } => Some(name.node.clone()),
            _ => {
                // Fallback: resolve the expression's type via the type system.
                // Handles variables, field accesses, method calls, etc.
                self.resolve_expr_type_id(inner_expr)
                    .map(|tid| c_types::type_id_to_c(tid, self.types, self.scopes))
            }
        }?;

        Some((trait_name, concrete_type, inner_expr))
    }

    /// Extract Box[Callable[sig]] / Box[MutCallable[sig]] / Box[ConsumeCallable[sig]] from an AST type.
    /// Returns (kind_prefix, return_type, params) if the type matches.
    fn extract_box_callable_type<'b>(
        type_: &'b Spanned<Type>,
    ) -> Option<(&'static str, &'b Spanned<Type>, &'b [Spanned<Type>], &'b [Ownership])> {
        if let Type::Named { name, generic_args } = &type_.node {
            if name.node == "Box" && generic_args.len() == 1 {
                if let Type::Named { name: inner_name, generic_args: inner_args } = &generic_args[0].node {
                    let kind_prefix = match inner_name.node.as_str() {
                        "Callable" => Some("Callable"),
                        "MutCallable" => Some("MutCallable"),
                        "ConsumeCallable" => Some("ConsumeCallable"),
                        _ => None,
                    };
                    if let Some(prefix) = kind_prefix {
                        if inner_args.len() == 1 {
                            if let Type::Function { return_type, params, param_ownerships } = &inner_args[0].node {
                                return Some((prefix, return_type, params, param_ownerships));
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Check if an iterable expression resolves to a GorgetArray type.
    pub(super) fn is_gorget_array_expr(&mut self, expr: &Spanned<Expr>) -> bool {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            let c_type = self.type_id_to_c_substituted(tid);
            if c_type == "GorgetArray" {
                return true;
            }
        }
        // Check if variable was auto-promoted from array literal to GorgetArray
        if let Expr::Identifier(name) = &expr.node {
            if self.vector_vars.contains(&crate::codegen::c_mangle::escape_keyword(name)) {
                return true;
            }
        }
        // Also check inferred type as fallback
        let c_type = self.infer_c_type_from_expr(&expr.node);
        c_type == "GorgetArray"
    }

    /// Check if an iterable expression resolves to a GorgetMap (Dict) type.
    pub(super) fn is_gorget_map_expr(&mut self, expr: &Spanned<Expr>) -> bool {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            if let crate::semantic::types::ResolvedType::Generic(def_id, _) = self.types.get(tid) {
                let def_name = &self.scopes.get_def(*def_id).name;
                return matches!(def_name.as_str(), "Dict" | "HashMap");
            }
        }
        false
    }

    /// Check if a map expression is an ordered Dict (vs unordered HashMap).
    pub(super) fn is_ordered_map_expr(&mut self, expr: &Spanned<Expr>) -> bool {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            if let crate::semantic::types::ResolvedType::Generic(def_id, _) = self.types.get(tid) {
                let def_name = &self.scopes.get_def(*def_id).name;
                return def_name == "Dict";
            }
        }
        false
    }

    /// Check if an iterable expression resolves to a GorgetSet (Set) type.
    /// Must use Gorget-level type name since GorgetSet is typedef'd to GorgetMap at C level.
    pub(super) fn is_gorget_set_expr(&mut self, expr: &Spanned<Expr>) -> bool {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            if let crate::semantic::types::ResolvedType::Generic(def_id, _) = self.types.get(tid) {
                let def_name = &self.scopes.get_def(*def_id).name;
                return matches!(def_name.as_str(), "Set" | "HashSet");
            }
        }
        false
    }

    /// Infer the element C type for a Set expression.
    pub(super) fn infer_set_elem_type(&mut self, expr: &Spanned<Expr>) -> String {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            if let crate::semantic::types::ResolvedType::Generic(_, args) = self.types.get(tid) {
                if let Some(&elem_tid) = args.first() {
                    return self.type_id_to_c_substituted(elem_tid);
                }
            }
        }
        "int64_t".to_string()
    }

    /// Infer the Gorget-level type name from an expression (e.g. "Counter").
    fn infer_type_name_from_expr(&mut self, expr: &Spanned<Expr>) -> String {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            match self.types.get(tid) {
                crate::semantic::types::ResolvedType::Defined(def_id) => {
                    return self.scopes.get_def(*def_id).name.clone();
                }
                crate::semantic::types::ResolvedType::Generic(def_id, _) => {
                    return self.scopes.get_def(*def_id).name.clone();
                }
                _ => {}
            }
        }
        if let Expr::StructLiteral { name, .. } = &expr.node {
            return name.node.clone();
        }
        if let Expr::Call { callee, .. } = &expr.node {
            if let Expr::Identifier(name) = &callee.node {
                return name.clone();
            }
        }
        // Fallback for FieldAccess on generic types: ast_type_to_type_id can't resolve
        // generic Named types like SparseSet[Health], so extract the base name directly
        // from the AST type annotation in field_type_names.
        if let Expr::FieldAccess { object, field } = &expr.node {
            let obj_type = self.infer_receiver_type(object);
            if obj_type != "Unknown" {
                let key = (obj_type, field.node.clone());
                if let Some(ast_type) = self.field_type_names.get(&key) {
                    if let Type::Named { name, .. } = ast_type {
                        return name.node.clone();
                    }
                }
            }
        }
        String::new()
    }

    /// Check if an expression's type has an Iterable[T] trait implementation.
    fn has_iterable_impl(&mut self, expr: &Spanned<Expr>) -> bool {
        let type_name = self.infer_type_name_from_expr(expr);
        if type_name.is_empty() {
            return false;
        }
        self.traits.impls.iter().any(|i|
            i.self_type_name == type_name && i.trait_name.as_deref() == Some("Iterable")
        )
    }

    /// Get info for an Iterable[T] implementation: (elem_c_type, iter_c_type, iter_type_name).
    fn get_iterable_iter_info(&mut self, expr: &Spanned<Expr>) -> (String, String, String) {
        let type_name = self.infer_type_name_from_expr(expr);
        for imp in &self.traits.impls {
            if imp.self_type_name == type_name && imp.trait_name.as_deref() == Some("Iterable") {
                // elem_c_type from the trait generic arg (e.g. int from Iterable[int])
                let elem_c_type = imp.trait_generic_args.first()
                    .map(|t| c_types::ast_type_to_c(t, self.scopes))
                    .unwrap_or_else(|| "int64_t".to_string());
                // iter type from the iter() method's return type
                if let Some((_def_id, sig)) = imp.methods.get("iter") {
                    let iter_type_name = match self.types.get(sig.return_type) {
                        crate::semantic::types::ResolvedType::Defined(def_id) => {
                            self.scopes.get_def(*def_id).name.clone()
                        }
                        crate::semantic::types::ResolvedType::Generic(def_id, _) => {
                            self.scopes.get_def(*def_id).name.clone()
                        }
                        _ => String::new(),
                    };
                    let iter_c_type = if iter_type_name.is_empty() {
                        "void".to_string()
                    } else {
                        iter_type_name.clone()
                    };
                    return (elem_c_type, iter_c_type, iter_type_name);
                }
            }
        }
        ("int64_t".to_string(), "void".to_string(), String::new())
    }

    /// Check if an iterable expression has an Iterator[T] trait implementation.
    fn has_iterator_impl(&mut self, expr: &Spanned<Expr>) -> bool {
        let type_name = self.infer_type_name_from_expr(expr);
        if type_name.is_empty() {
            return false;
        }
        self.traits.impls.iter().any(|i|
            i.self_type_name == type_name && i.trait_name.as_deref() == Some("Iterator")
        )
    }

    /// Get the element C type for an Iterator[T] implementation.
    pub(crate) fn get_iterator_elem_c_type(&mut self, expr: &Spanned<Expr>) -> String {
        let type_name = self.infer_type_name_from_expr(expr);
        for imp in &self.traits.impls {
            if imp.self_type_name == type_name && imp.trait_name.as_deref() == Some("Iterator") {
                if let Some(first_arg) = imp.trait_generic_args.first() {
                    return c_types::ast_type_to_c(first_arg, self.scopes);
                }
            }
        }
        "int64_t".to_string()
    }

    /// Generate a for loop over a range or iterable, with optional else clause.
    fn gen_for_loop(
        &mut self,
        pattern: &Spanned<Pattern>,
        iterable: &Spanned<Expr>,
        body: &Block,
        else_body: &Option<Block>,
        emitter: &mut CEmitter,
    ) {
        // Detect .enumerate() and unwrap to inner iterable
        let (iterable, enumerate_idx_var) = if let Expr::MethodCall {
            receiver, method, args, ..
        } = &iterable.node {
            if method.node == "enumerate" && args.is_empty() {
                if let Pattern::Tuple(elems) = &pattern.node {
                    let idx_name = match &elems[0].node {
                        Pattern::Binding(n) => c_mangle::escape_keyword(n),
                        _ => "__gorget_enum_idx".to_string(),
                    };
                    (receiver.as_ref(), Some(idx_name))
                } else {
                    (iterable, None)
                }
            } else {
                (iterable, None)
            }
        } else {
            (iterable, None)
        };

        // Extract the element variable name (second element of tuple when enumerating)
        let gorget_var_name = if enumerate_idx_var.is_some() {
            if let Pattern::Tuple(elems) = &pattern.node {
                if elems.len() >= 2 {
                    match &elems[1].node {
                        Pattern::Binding(name) => name.clone(),
                        _ => "__gorget_elem".to_string(),
                    }
                } else { "__gorget_elem".to_string() }
            } else { "__gorget_i".to_string() }
        } else {
            match &pattern.node {
                Pattern::Binding(name) => name.clone(),
                _ => "__gorget_i".to_string(),
            }
        };
        let var_name = if enumerate_idx_var.is_some() {
            if let Pattern::Tuple(elems) = &pattern.node {
                if elems.len() >= 2 {
                    match &elems[1].node {
                        Pattern::Binding(name) => c_mangle::escape_keyword(name),
                        _ => "__gorget_elem".to_string(),
                    }
                } else { "__gorget_elem".to_string() }
            } else { "__gorget_i".to_string() }
        } else {
            match &pattern.node {
                Pattern::Binding(name) => c_mangle::escape_keyword(name),
                _ => "__gorget_i".to_string(),
            }
        };

        // Else handling: emit a break-flag before the loop
        let flag = else_body.as_ref().map(|_| {
            let f = emitter.fresh_temp();
            emitter.emit_line(&format!("bool {f} = false;"));
            f
        });
        let has_else = flag.is_some();

        // Enumerate counter: emit before the loop (unique name per loop)
        let enum_counter = if enumerate_idx_var.is_some() {
            let c = emitter.fresh_temp();
            emitter.emit_line(&format!("size_t {c} = 0;"));
            Some(c)
        } else {
            None
        };

        // Track the loop kind for trace output
        let is_range = matches!(iterable.node, Expr::Range { .. });
        // Track whether var_name holds the element (true) or pattern is already handled (false)
        let mut var_name_holds_element = true;

        // --- Iterable-specific: preamble, loop header, indent, inner preamble, bindings ---

        if let Expr::Range {
            start,
            end,
            inclusive,
        } = &iterable.node
        {
            let start_expr = start
                .as_ref()
                .map(|e| self.gen_expr(e))
                .unwrap_or_else(|| "0".to_string());
            let end_expr = end
                .as_ref()
                .map(|e| self.gen_expr(e))
                .unwrap_or_else(|| "0".to_string());
            let cmp = if *inclusive { "<=" } else { "<" };
            emitter.emit_line(&format!(
                "for (int64_t {var_name} = {start_expr}; {var_name} {cmp} {end_expr}; {var_name}++) {{"
            ));
            emitter.indent();
            if !has_else { self.push_drop_scope(DropScopeKind::Loop); }
            var_name_holds_element = false; // var_name IS the loop counter, not an element to destructure
        } else if self.is_string_expr(iterable) {
            let iter = self.gen_expr(iterable);
            let len_var = emitter.fresh_temp();
            let idx = emitter.fresh_temp();
            emitter.emit_line(&format!("size_t {len_var} = strlen({iter});"));
            emitter.emit_line(&format!(
                "for (size_t {idx} = 0; {idx} < {len_var}; {idx}++) {{"
            ));
            emitter.indent();
            if !has_else { self.push_drop_scope(DropScopeKind::Loop); }
            emitter.emit_line(&format!("char {var_name} = {iter}[{idx}];"));
            var_name_holds_element = false; // char element, no tuple destructuring
        } else if self.is_gorget_array_expr(iterable) {
            let iter = self.gen_expr(iterable);
            let idx = emitter.fresh_temp();
            let elem_type = self.infer_vector_elem_type(iterable);
            emitter.emit_line(&format!(
                "for (size_t {idx} = 0; {idx} < gorget_array_len(&{iter}); {idx}++) {{"
            ));
            emitter.indent();
            if !has_else { self.push_drop_scope(DropScopeKind::Loop); }
            emitter.emit_line(&format!(
                "{elem_type} {var_name} = GORGET_ARRAY_AT({elem_type}, {iter}, {idx});"
            ));
        } else if self.is_gorget_map_expr(iterable) {
            let ordered = self.is_ordered_map_expr(iterable);
            let iter = self.gen_expr(iterable);
            let idx = emitter.fresh_temp();
            let (key_type, val_type) = self.infer_map_kv_types(iterable);
            if ordered {
                let oi = emitter.fresh_temp();
                emitter.emit_line(&format!(
                    "for (size_t {oi} = 0; {oi} < {iter}.order_len; {oi}++) {{"
                ));
                emitter.indent();
                emitter.emit_line(&format!("size_t {idx} = {iter}.order[{oi}];"));
                emitter.emit_line(&format!("if ({iter}.states[{idx}] != 1) continue;"));
            } else {
                emitter.emit_line(&format!(
                    "for (size_t {idx} = 0; {idx} < {iter}.cap; {idx}++) {{"
                ));
                emitter.indent();
                emitter.emit_line(&format!("if ({iter}.states[{idx}] != 1) continue;"));
            }
            if !has_else { self.push_drop_scope(DropScopeKind::Loop); }
            if let Pattern::Tuple(elems) = &pattern.node {
                let k_name = match &elems[0].node {
                    Pattern::Binding(name) => c_mangle::escape_keyword(name),
                    _ => "__gorget_k".to_string(),
                };
                let v_name = if elems.len() >= 2 {
                    match &elems[1].node {
                        Pattern::Binding(name) => c_mangle::escape_keyword(name),
                        _ => "__gorget_v".to_string(),
                    }
                } else {
                    "__gorget_v".to_string()
                };
                emitter.emit_line(&format!(
                    "{key_type} {k_name} = {iter}.keys[{idx}];"
                ));
                emitter.emit_line(&format!(
                    "{val_type} {v_name} = {iter}.values[{idx}];"
                ));
            } else {
                emitter.emit_line(&format!(
                    "{key_type} {var_name} = {iter}.keys[{idx}];"
                ));
            }
            var_name_holds_element = false; // Map handles (k,v) directly
        } else if self.is_gorget_set_expr(iterable) {
            let iter = self.gen_expr(iterable);
            let idx = emitter.fresh_temp();
            let elem_type = self.infer_set_elem_type(iterable);
            emitter.emit_line(&format!(
                "for (size_t {idx} = 0; {idx} < {iter}.cap; {idx}++) {{"
            ));
            emitter.indent();
            emitter.emit_line(&format!("if ({iter}.states[{idx}] != 1) continue;"));
            if !has_else { self.push_drop_scope(DropScopeKind::Loop); }
            emitter.emit_line(&format!(
                "{elem_type} {var_name} = *({elem_type}*)((char*){iter}.keys + {idx} * {iter}.key_size);"
            ));
        } else if self.has_iterable_impl(iterable) {
            // Iterable[T]: call iter() to get a fresh iterator, then use Iterator protocol
            let collection_expr = self.gen_expr(iterable);
            // Use infer_receiver_mangled_type for the fully-qualified name (e.g. "SparseSet__Health")
            // since trait method functions are emitted per-monomorphization.
            let collection_mangled = self.infer_receiver_mangled_type(iterable);
            let (elem_c_type, iter_c_type, iter_type_name) = self.get_iterable_iter_info(iterable);
            let iterable_trait_args = self.lookup_trait_type_args(&collection_mangled, "Iterable");
            let iter_fn = c_mangle::mangle_trait_method("Iterable", &collection_mangled, "iter", &iterable_trait_args);
            let option_mangled = self.register_generic("Option", &[elem_c_type.clone()], super::GenericInstanceKind::Enum);
            let tag_none = c_mangle::mangle_tag(&option_mangled, "None");
            let iterator_trait_args = self.lookup_trait_type_args(&iter_type_name, "Iterator");
            let next_fn = c_mangle::mangle_trait_method("Iterator", &iter_type_name, "next", &iterator_trait_args);
            let iter_tmp = emitter.fresh_temp();
            let next_tmp = emitter.fresh_temp();
            emitter.emit_line(&format!("{iter_c_type} {iter_tmp} = {iter_fn}(&{collection_expr});"));
            emitter.emit_line("while (1) {");
            emitter.indent();
            if !has_else { self.push_drop_scope(DropScopeKind::Loop); }
            emitter.emit_line(&format!("{option_mangled} {next_tmp} = {next_fn}(&{iter_tmp});"));
            emitter.emit_line(&format!("if ({next_tmp}.tag == {tag_none}) break;"));
            emitter.emit_line(&format!("{elem_c_type} {var_name} = {next_tmp}.data.Some._0;"));
        } else if self.has_iterator_impl(iterable) {
            let iter_expr = self.gen_expr(iterable);
            let type_mangled = self.infer_receiver_type(iterable);
            let elem_c_type = self.get_iterator_elem_c_type(iterable);
            let option_mangled = self.register_generic("Option", &[elem_c_type.clone()], super::GenericInstanceKind::Enum);
            let tag_none = c_mangle::mangle_tag(&option_mangled, "None");
            let iterator_trait_args = self.lookup_trait_type_args(&type_mangled, "Iterator");
            let next_fn = c_mangle::mangle_trait_method("Iterator", &type_mangled, "next", &iterator_trait_args);
            let next_tmp = emitter.fresh_temp();
            emitter.emit_line("while (1) {");
            emitter.indent();
            if !has_else { self.push_drop_scope(DropScopeKind::Loop); }
            emitter.emit_line(&format!("{option_mangled} {next_tmp} = {next_fn}(&{iter_expr});"));
            emitter.emit_line(&format!("if ({next_tmp}.tag == {tag_none}) break;"));
            emitter.emit_line(&format!("{elem_c_type} {var_name} = {next_tmp}.data.Some._0;"));
        } else {
            // For-in over C array
            let iter = self.gen_expr(iterable);
            let idx = emitter.fresh_temp();
            emitter.emit_line(&format!(
                "for (size_t {idx} = 0; {idx} < sizeof({iter})/sizeof({iter}[0]); {idx}++) {{"
            ));
            emitter.indent();
            if !has_else { self.push_drop_scope(DropScopeKind::Loop); }
            emitter.emit_line(&format!(
                "__typeof__({iter}[0]) {var_name} = {iter}[{idx}];"
            ));
        }

        // --- Enumerate: bind the sequential index ---
        if let (Some(idx_name), Some(counter)) = (&enumerate_idx_var, &enum_counter) {
            emitter.emit_line(&format!("int64_t {idx_name} = (int64_t){counter}++;"));
        }

        // --- Common: trace loop iteration ---
        if self.trace && is_range {
            let s = format!(
                r#"fprintf(__gorget_trace_fp, "{{\"type\":\"loop\",\"kind\":\"for\",\"var\":\"{gorget_var_name}\",\"value\":");"#
            );
            emitter.emit_line(&s);
            emitter.emit_line(&format!(
                "__gorget_trace_val_int(__gorget_trace_fp, {var_name});"
            ));
            emitter.emit_line(
                r#"fprintf(__gorget_trace_fp, ",\"depth\":%d}\n", __gorget_trace_depth);"#
            );
        } else if self.trace {
            // For collection loops, emit var and value using _Generic
            let s = format!(
                r#"fprintf(__gorget_trace_fp, "{{\"type\":\"loop\",\"kind\":\"for\",\"var\":\"{gorget_var_name}\",\"value\":");"#
            );
            emitter.emit_line(&s);
            emitter.emit_line(&format!(
                "_Generic(({var_name}), \
                 int64_t: __gorget_trace_val_int, \
                 long: __gorget_trace_val_int, \
                 double: __gorget_trace_val_float, \
                 float: __gorget_trace_val_float, \
                 _Bool: __gorget_trace_val_bool, \
                 char: __gorget_trace_val_char, \
                 const char*: __gorget_trace_val_str, \
                 char*: __gorget_trace_val_str, \
                 default: __gorget_trace_val_int\
                 )(__gorget_trace_fp, {var_name});"
            ));
            emitter.emit_line(
                r#"fprintf(__gorget_trace_fp, ",\"depth\":%d}\n", __gorget_trace_depth);"#
            );
        }

        // --- Common: pattern destructuring for non-binding patterns ---
        // Only emit when var_name holds the element and pattern needs unpacking
        if var_name_holds_element {
            if enumerate_idx_var.is_some() {
                // Enumerate: destructure the element part (second tuple field) if non-trivial
                if let Pattern::Tuple(elems) = &pattern.node {
                    if elems.len() >= 2 && !matches!(&elems[1].node, Pattern::Binding(_)) {
                        self.emit_pattern_bindings(&elems[1].node, &var_name, emitter);
                    }
                }
            } else if !matches!(&pattern.node, Pattern::Binding(_)) {
                self.emit_pattern_bindings(&pattern.node, &var_name, emitter);
            }
        }

        // --- Common: body ---
        if let Some(ref flag) = flag {
            self.gen_block_with_break_flag(body, flag, emitter);
        } else {
            self.gen_block(body, emitter);
            self.pop_drop_scope(emitter);
        }

        // --- Common: close loop ---
        emitter.dedent();
        emitter.emit_line("}");

        // --- Common: else block ---
        if let (Some(flag), Some(else_block)) = (&flag, else_body) {
            emitter.emit_line(&format!("if (!{flag}) {{"));
            emitter.indent();
            self.gen_block(else_block, emitter);
            emitter.dedent();
            emitter.emit_line("}");
        }
    }

    /// Generate a match statement.
    fn gen_match_stmt(
        &mut self,
        scrutinee: &Spanned<Expr>,
        arms: &[crate::parser::ast::MatchArm],
        else_arm: &Option<Block>,
        emitter: &mut CEmitter,
    ) {
        let scrut_expr = self.gen_expr(scrutinee);
        let scrut_temps = self.flush_string_temps(emitter);

        // Always evaluate scrutinee into a temp to avoid double-evaluation.
        // When the scrutinee is `self` inside a method body, `self` is a pointer
        // (const T *self), so we must dereference it to get the struct value.
        let tmp = emitter.fresh_temp();
        let is_self_scrutinee = self.current_self_type.is_some() && matches!(scrutinee.node, Expr::SelfExpr);
        if is_self_scrutinee {
            emitter.emit_line(&format!(
                "__typeof__(*{scrut_expr}) {tmp} = *{scrut_expr};"
            ));
        } else {
            emitter.emit_line(&format!(
                "__typeof__({scrut_expr}) {tmp} = {scrut_expr};"
            ));
        }

        // Resolve the mangled enum type name for generic enums (e.g. Option__int64_t)
        let enum_c_type = self.resolve_enum_c_type_for_scrutinee(scrutinee);

        // Use if-else chain for all patterns (simpler, handles all cases)
        let mut first = true;
        for arm in arms {
            let pattern_cond = self.pattern_to_condition(&arm.pattern.node, &tmp, enum_c_type.as_deref());

            // Apply guard expression if present.
            // When a guard references pattern bindings (e.g. `case n if n > 0`),
            // we must declare them before the guard is evaluated.  We wrap bindings
            // + guard in a GCC statement expression so they live in the condition.
            let full_cond = if let Some(guard) = &arm.guard {
                let guard_expr = self.gen_expr(guard);
                let bindings = self.stmt_pattern_bindings_inline(&arm.pattern.node, &tmp);
                if bindings.is_empty() {
                    format!("({pattern_cond}) && ({guard_expr})")
                } else {
                    format!("({pattern_cond}) && ({{ {bindings}({guard_expr}); }})")
                }
            } else {
                pattern_cond
            };

            if first {
                emitter.emit_line(&format!("if ({full_cond}) {{"));
                first = false;
            } else {
                emitter.emit_line(&format!("}} else if ({full_cond}) {{"));
            }
            emitter.indent();

            // Emit pattern bindings inside the arm body
            self.emit_pattern_bindings(&arm.pattern.node, &tmp, emitter);
            self.register_pattern_var_types(&arm.pattern.node, scrutinee);

            // If the arm body is a block, generate it as statements directly
            // (avoids wrapping in a GCC statement expression which can't handle
            // nested match/if statements that only contain returns).
            self.push_drop_scope(DropScopeKind::Block);
            if let Expr::Block(block) = &arm.body.node {
                self.gen_block(block, emitter);
            } else {
                let body = self.gen_expr(&arm.body);
                emitter.emit_line(&format!("{body};"));
            }
            self.pop_drop_scope(emitter);
            emitter.dedent();
        }
        if let Some(else_body) = else_arm {
            emitter.emit_line("} else {");
            emitter.indent();
            self.push_drop_scope(DropScopeKind::Block);
            self.gen_block(else_body, emitter);
            self.pop_drop_scope(emitter);
            emitter.dedent();
        }
        if !arms.is_empty() || else_arm.is_some() {
            emitter.emit_line("}");
        }
        Self::emit_string_temp_frees(&scrut_temps, emitter);
    }

    /// Convert a pattern to a C boolean condition (for if-else chain).
    /// `enum_c_type` overrides the enum name used for tag constants (needed for generic enums
    /// where the monomorphized name like `Option__int64_t` differs from the raw name `Option`).
    pub(super) fn pattern_to_condition(&mut self, pattern: &Pattern, scrutinee: &str, enum_c_type: Option<&str>) -> String {
        match pattern {
            Pattern::Literal(lit) if matches!(lit.node, Expr::NoneLiteral) => {
                // None literal as pattern: generate tag check for the None variant
                if let Some((enum_name, _)) = self.find_enum_for_variant_by_name("None") {
                    let effective = enum_c_type.unwrap_or(&enum_name);
                    let tag = c_mangle::mangle_tag(effective, "None");
                    format!("{scrutinee}.tag == {tag}")
                } else {
                    let val = self.gen_expr(lit);
                    format!("{scrutinee} == {val}")
                }
            }
            Pattern::Literal(lit) => {
                let val = self.gen_expr(lit);
                format!("{scrutinee} == {val}")
            }
            Pattern::Wildcard => "1".to_string(),
            Pattern::Binding(name) => {
                // A bare identifier may be a unit enum variant (parser can't distinguish).
                if let Some((enum_name, _)) = self.find_enum_for_variant_by_name(name) {
                    let effective = enum_c_type.unwrap_or(&enum_name);
                    let tag = c_mangle::mangle_tag(effective, name);
                    format!("{scrutinee}.tag == {tag}")
                } else {
                    "1".to_string() // genuine binding — always matches
                }
            }
            Pattern::Constructor { path, fields: _ } => {
                // Enum variant destructuring: check the tag
                if let Some((enum_name, variant_name)) = self.find_enum_for_variant_path(path) {
                    let effective = enum_c_type.unwrap_or(&enum_name);
                    let tag = c_mangle::mangle_tag(effective, &variant_name);
                    format!("{scrutinee}.tag == {tag}")
                } else {
                    "1".to_string()
                }
            }
            Pattern::Or(alternatives) => {
                let conds: Vec<String> = alternatives
                    .iter()
                    .map(|p| self.pattern_to_condition(&p.node, scrutinee, enum_c_type))
                    .collect();
                format!("({})", conds.join(" || "))
            }
            Pattern::Tuple(_) => "1".to_string(), // tuple always matches structurally
            Pattern::Rest => "1".to_string(),
        }
    }

    /// Flatten a `BinaryOp::And` chain into an ordered list of sub-expressions (left-to-right).
    fn flatten_and_chain<'a>(expr: &'a Spanned<Expr>) -> Vec<&'a Spanned<Expr>> {
        let mut parts = Vec::new();
        Self::flatten_and_chain_inner(expr, &mut parts);
        parts
    }

    fn flatten_and_chain_inner<'a>(expr: &'a Spanned<Expr>, parts: &mut Vec<&'a Spanned<Expr>>) {
        if let Expr::BinaryOp { left, op: BinaryOp::And, right } = &expr.node {
            Self::flatten_and_chain_inner(left, parts);
            Self::flatten_and_chain_inner(right, parts);
        } else {
            parts.push(expr);
        }
    }

    /// Generate codegen for `if expr is Pattern(bindings):` and compound conditions
    /// like `if a is Some(x) and x > 0:` or `if a is Some(x) and b is Ok(y) and guard:`.
    fn gen_if_is_binding(
        &mut self,
        condition: &Spanned<Expr>,
        then_body: &Block,
        elif_branches: &[(Spanned<Expr>, Block)],
        else_body: &Option<Block>,
        emitter: &mut CEmitter,
    ) {
        let all_scrut_temps = self.gen_compound_is_condition(condition, "if", emitter);

        self.push_drop_scope(DropScopeKind::Block);
        self.gen_block(then_body, emitter);
        self.pop_drop_scope(emitter);
        emitter.dedent();

        let mut wrapper_temp_frees: Vec<Vec<String>> = Vec::new();

        for (elif_cond, elif_body) in elif_branches {
            let elif_is_binding = Self::has_compound_is_bindings(&elif_cond.node);
            if elif_is_binding {
                emitter.emit_line("} else {");
                emitter.indent();
                let scrut_temps = self.gen_compound_is_condition(elif_cond, "elif", emitter);
                self.push_drop_scope(DropScopeKind::Block);
                self.gen_block(elif_body, emitter);
                self.pop_drop_scope(emitter);
                emitter.dedent();
                wrapper_temp_frees.push(scrut_temps);
            } else {
                let ec = self.gen_expr(elif_cond);
                let elif_temps = self.flush_string_temps_no_emit();
                if elif_temps.is_empty() {
                    emitter.emit_line(&format!("}} else if ({ec}) {{"));
                    emitter.indent();
                    if self.trace {
                        let src = c_string_escape(&format!("elif {}", self.source_line(elif_cond.span)));
                        let vars = self.collect_expr_vars(&[elif_cond]);
                        self.emit_branch_trace("elif", &src, &vars, emitter);
                    }
                    self.push_drop_scope(DropScopeKind::Block);
                    self.gen_block(elif_body, emitter);
                    self.pop_drop_scope(emitter);
                    emitter.dedent();
                } else {
                    emitter.emit_line("} else {");
                    emitter.indent();
                    for (name, expr) in &elif_temps {
                        emitter.emit_line(&format!("GorgetString {name} = {expr};"));
                    }
                    emitter.emit_line(&format!("if ({ec}) {{"));
                    emitter.indent();
                    self.push_drop_scope(DropScopeKind::Block);
                    self.gen_block(elif_body, emitter);
                    self.pop_drop_scope(emitter);
                    emitter.dedent();
                    let names: Vec<String> = elif_temps.iter().map(|(n, _)| n.clone()).collect();
                    wrapper_temp_frees.push(names);
                }
            }
        }

        if let Some(else_body) = else_body {
            emitter.emit_line("} else {");
            emitter.indent();
            if self.trace {
                self.emit_branch_trace("else", "else", &[], emitter);
            }
            self.push_drop_scope(DropScopeKind::Block);
            self.gen_block(else_body, emitter);
            self.pop_drop_scope(emitter);
            emitter.dedent();
        }

        emitter.emit_line("}");
        for temps in wrapper_temp_frees.iter().rev() {
            Self::emit_string_temp_frees(temps, emitter);
            emitter.dedent();
            emitter.emit_line("}");
        }
        Self::emit_string_temp_frees(&all_scrut_temps, emitter);
    }

    /// Shared helper for generating a compound `is` condition. Evaluates all scrutinee temps,
    /// builds the combined condition (tag checks + guards via stmt-expr if needed), emits
    /// the `if (condition) {` line with pattern bindings, and returns all scrutinee string temps.
    /// Leaves the emitter indented inside the if block.
    fn gen_compound_is_condition(
        &mut self,
        condition: &Spanned<Expr>,
        branch_label: &str,
        emitter: &mut CEmitter,
    ) -> Vec<String> {
        let parts = Self::flatten_and_chain(condition);

        // Phase 1: evaluate all scrutinee temps and collect info
        struct IsInfo {
            tmp: String,
            tag_cond: String,
            has_bindings: bool,
        }
        let mut all_scrut_temps = Vec::new();
        let mut is_infos: Vec<IsInfo> = Vec::new();
        let mut guard_indices: Vec<usize> = Vec::new(); // indices of non-is parts

        for (i, part) in parts.iter().enumerate() {
            if let Expr::Is { expr: scrutinee, negated: false, pattern, .. } = &part.node {
                let scrut_expr = self.gen_expr(scrutinee);
                let scrut_temps = self.flush_string_temps(emitter);
                all_scrut_temps.extend(scrut_temps);
                let tmp = emitter.fresh_temp();
                emitter.emit_line(&format!("__typeof__({scrut_expr}) {tmp} = {scrut_expr};"));

                let enum_c_type = self.resolve_enum_c_type_for_scrutinee(scrutinee);
                let tag_cond = self.pattern_to_condition(&pattern.node, &tmp, enum_c_type.as_deref());
                let has_bindings = Self::has_pattern_bindings(&pattern.node);

                is_infos.push(IsInfo { tmp, tag_cond, has_bindings });
            } else {
                guard_indices.push(i);
            }
        }

        // Phase 2: build the combined condition
        let mut cond_parts: Vec<String> = Vec::new();
        for info in &is_infos {
            cond_parts.push(info.tag_cond.clone());
        }

        // If there are guard expressions, they might reference pattern bindings.
        // Wrap them in a GCC statement expression with inline bindings.
        if !guard_indices.is_empty() {
            let mut inline_bindings = String::new();
            let mut info_idx = 0;
            for part in &parts {
                if let Expr::Is { pattern, negated: false, .. } = &part.node {
                    if info_idx < is_infos.len() && is_infos[info_idx].has_bindings {
                        inline_bindings.push_str(&self.stmt_pattern_bindings_inline(
                            &pattern.node,
                            &is_infos[info_idx].tmp,
                        ));
                    }
                    info_idx += 1;
                }
            }

            let guard_exprs: Vec<String> = guard_indices.iter()
                .map(|&i| self.gen_expr(parts[i]))
                .collect();
            let guard_combined = guard_exprs.join(" && ");

            if inline_bindings.is_empty() {
                cond_parts.push(guard_combined);
            } else {
                cond_parts.push(format!("({{ {inline_bindings}({guard_combined}); }})"));
            }
        }

        let full_cond = cond_parts.join(" && ");
        emitter.emit_line(&format!("if ({full_cond}) {{"));
        emitter.indent();

        if self.trace {
            let src = c_string_escape(&format!("{branch_label} {}", self.source_line(condition.span)));
            let vars = self.collect_expr_vars(&[condition]);
            self.emit_branch_trace(branch_label, &src, &vars, emitter);
        }

        // Phase 3: emit pattern bindings in the body scope
        let mut info_idx = 0;
        for part in &parts {
            if let Expr::Is { pattern, negated: false, .. } = &part.node {
                if info_idx < is_infos.len() && is_infos[info_idx].has_bindings {
                    self.emit_pattern_bindings(&pattern.node, &is_infos[info_idx].tmp, emitter);
                }
                info_idx += 1;
            }
        }

        all_scrut_temps
    }

    /// Generate codegen for `while expr is Pattern(bindings):` with pattern variable extraction.
    fn gen_while_is_binding(
        &mut self,
        condition: &Spanned<Expr>,
        body: &Block,
        else_body: Option<&Block>,
        emitter: &mut CEmitter,
    ) {
        if let Expr::Is { expr: scrutinee, pattern, .. } = &condition.node {
            let trace_iter = if self.trace {
                let t = emitter.fresh_temp();
                emitter.emit_line(&format!("int64_t {t} = 0;"));
                Some(t)
            } else {
                None
            };

            if let Some(else_block) = else_body {
                let flag = emitter.fresh_temp();
                emitter.emit_line(&format!("bool {flag} = false;"));
                emitter.emit_line("while (1) {");
                emitter.indent();
                self.push_drop_scope(DropScopeKind::Loop);

                // Evaluate scrutinee each iteration
                let scrut_expr = self.gen_expr(scrutinee);
                let scrut_temps = self.flush_string_temps(emitter);
                let tmp = emitter.fresh_temp();
                emitter.emit_line(&format!("__typeof__({scrut_expr}) {tmp} = {scrut_expr};"));

                let enum_c_type = self.resolve_enum_c_type_for_scrutinee(scrutinee);
                let cond = self.pattern_to_condition(&pattern.node, &tmp, enum_c_type.as_deref());
                emitter.emit_line(&format!("if (!({cond})) break;"));

                self.emit_pattern_bindings(&pattern.node, &tmp, emitter);
                if let Some(ref ti) = trace_iter {
                    self.emit_while_iter_trace(ti, emitter);
                }
                self.gen_block_with_break_flag(body, &flag, emitter);
                Self::emit_string_temp_frees(&scrut_temps, emitter);
                self.pop_drop_scope(emitter);
                emitter.dedent();
                emitter.emit_line("}");
                emitter.emit_line(&format!("if (!{flag}) {{"));
                emitter.indent();
                self.gen_block(else_block, emitter);
                emitter.dedent();
                emitter.emit_line("}");
            } else {
                emitter.emit_line("while (1) {");
                emitter.indent();
                self.push_drop_scope(DropScopeKind::Loop);

                let scrut_expr = self.gen_expr(scrutinee);
                let scrut_temps = self.flush_string_temps(emitter);
                let tmp = emitter.fresh_temp();
                emitter.emit_line(&format!("__typeof__({scrut_expr}) {tmp} = {scrut_expr};"));

                let enum_c_type = self.resolve_enum_c_type_for_scrutinee(scrutinee);
                let cond = self.pattern_to_condition(&pattern.node, &tmp, enum_c_type.as_deref());
                emitter.emit_line(&format!("if (!({cond})) break;"));

                self.emit_pattern_bindings(&pattern.node, &tmp, emitter);
                if let Some(ref ti) = trace_iter {
                    self.emit_while_iter_trace(ti, emitter);
                }
                self.gen_block(body, emitter);
                Self::emit_string_temp_frees(&scrut_temps, emitter);
                self.pop_drop_scope(emitter);
                emitter.dedent();
                emitter.emit_line("}");
            }
        }
    }

    /// Check if a pattern contains any `Binding` variants that need variable declarations.
    fn has_pattern_bindings(pattern: &Pattern) -> bool {
        match pattern {
            Pattern::Binding(name) => {
                // A bare identifier that matches an enum variant is not a real binding
                !matches!(name.as_str(), "None" | "True" | "False")
            }
            Pattern::Constructor { fields, .. } => {
                fields.iter().any(|f| Self::has_pattern_bindings(&f.node))
            }
            Pattern::Tuple(elements) => {
                elements.iter().any(|e| Self::has_pattern_bindings(&e.node))
            }
            Pattern::Or(alts) => {
                alts.iter().any(|a| Self::has_pattern_bindings(&a.node))
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => false,
        }
    }

    /// Check if a condition expression contains any `is` sub-expressions with bindings,
    /// walking through `BinaryOp::And` chains for compound conditions.
    fn has_compound_is_bindings(expr: &Expr) -> bool {
        match expr {
            Expr::Is { negated: false, pattern, .. } => Self::has_pattern_bindings(&pattern.node),
            Expr::BinaryOp { left, op: BinaryOp::And, right } => {
                Self::has_compound_is_bindings(&left.node)
                    || Self::has_compound_is_bindings(&right.node)
            }
            _ => false,
        }
    }

    /// Emit variable bindings from a pattern into the current scope.
    pub(super) fn emit_pattern_bindings(&mut self, pattern: &Pattern, scrutinee: &str, emitter: &mut CEmitter) {
        match pattern {
            Pattern::Binding(name) => {
                let escaped = c_mangle::escape_keyword(name);
                emitter.emit_line(&format!(
                    "__typeof__({scrutinee}) {escaped} = {scrutinee};"
                ));
            }
            Pattern::Constructor { path, fields } => {
                if let Some((_enum_name, variant_name)) = self.find_enum_for_variant_path(path) {
                    for (i, field_pat) in fields.iter().enumerate() {
                        let field_access =
                            format!("{scrutinee}.data.{variant_name}._{i}");
                        self.emit_pattern_bindings(&field_pat.node, &field_access, emitter);
                    }
                }
            }
            Pattern::Tuple(elements) => {
                for (i, elem) in elements.iter().enumerate() {
                    let field_access = format!("{scrutinee}._{i}");
                    self.emit_pattern_bindings(&elem.node, &field_access, emitter);
                }
            }
            Pattern::Or(alternatives) => {
                // For or-patterns, bind from the first alternative (they must all bind the same names)
                if let Some(first) = alternatives.first() {
                    self.emit_pattern_bindings(&first.node, scrutinee, emitter);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {
                // No bindings to emit
            }
        }
    }

    /// Emit pattern bindings with concrete C types (resolved from AST Type).
    /// Unlike `emit_pattern_bindings` which uses `__typeof__`, this emits explicit types
    /// so that downstream codegen (print format inference, etc.) can resolve variable types.
    fn emit_typed_pattern_bindings(
        &mut self,
        pattern: &Pattern,
        scrutinee: &str,
        ast_type: &crate::parser::ast::Type,
        emitter: &mut CEmitter,
    ) {
        use crate::parser::ast::Type;
        match (pattern, ast_type) {
            (Pattern::Binding(name), _) => {
                let escaped = c_mangle::escape_keyword(name);
                let c_type = self.type_to_c(ast_type);
                let decl = c_types::c_declare(&c_type, &escaped);
                emitter.emit_line(&format!("{decl} = {scrutinee};"));
                if let Some(tid) = self.ast_type_to_type_id(ast_type) {
                    self.pattern_var_types.insert(name.clone(), tid);
                }
            }
            (Pattern::Tuple(elements), Type::Tuple(types)) => {
                for (i, elem) in elements.iter().enumerate() {
                    let field_access = format!("{scrutinee}._{i}");
                    if let Some(field_type) = types.get(i) {
                        self.emit_typed_pattern_bindings(&elem.node, &field_access, &field_type.node, emitter);
                    } else {
                        self.emit_pattern_bindings(&elem.node, &field_access, emitter);
                    }
                }
            }
            (Pattern::Constructor { path, fields }, _) => {
                // Fall back to __typeof__ for constructor patterns (enum variants)
                if let Some((_enum_name, variant_name)) = self.find_enum_for_variant_path(path) {
                    for (i, field_pat) in fields.iter().enumerate() {
                        let field_access = format!("{scrutinee}.data.{variant_name}._{i}");
                        self.emit_pattern_bindings(&field_pat.node, &field_access, emitter);
                    }
                }
            }
            (Pattern::Or(alternatives), _) => {
                if let Some(first) = alternatives.first() {
                    self.emit_typed_pattern_bindings(&first.node, scrutinee, ast_type, emitter);
                }
            }
            _ => {
                // Wildcard, Literal, Rest, or type mismatch — fall back
                self.emit_pattern_bindings(pattern, scrutinee, emitter);
            }
        }
    }

    /// Register TypeIds for variables bound by pattern matching on enum variants.
    /// This fills `pattern_var_types` so downstream codegen (method calls, print, etc.)
    /// can resolve the concrete type of pattern-bound variables.
    pub(super) fn register_pattern_var_types(
        &mut self,
        pattern: &Pattern,
        scrutinee: &Spanned<Expr>,
    ) {
        let Some(scrut_tid) = self.resolve_expr_type_id(scrutinee) else { return };

        // Determine the enum's DefId and build a generic substitution map.
        let (enum_def_id, subst_tids) = match self.types.get(scrut_tid) {
            crate::semantic::types::ResolvedType::Generic(def_id, args) => (*def_id, args.clone()),
            crate::semantic::types::ResolvedType::Defined(def_id) => (*def_id, vec![]),
            _ => return,
        };

        let Some(info) = self.enum_variants.get(&enum_def_id).cloned() else { return };

        // Build AST-level substitution: generic param name → AST Type
        let subst: std::collections::HashMap<String, Type> = info
            .generic_param_names
            .iter()
            .zip(subst_tids.iter())
            .filter_map(|(param_name, &tid)| {
                self.type_id_to_ast_type(tid).map(|ty| (param_name.clone(), ty))
            })
            .collect();

        self.register_pattern_var_types_inner(pattern, &info, &subst);
    }

    /// Recursive inner helper for `register_pattern_var_types`.
    fn register_pattern_var_types_inner(
        &mut self,
        pattern: &Pattern,
        info: &crate::semantic::resolve::EnumVariantInfo,
        subst: &std::collections::HashMap<String, Type>,
    ) {
        match pattern {
            Pattern::Constructor { path, fields } => {
                let variant_name = if path.len() == 1 {
                    &path[0].node
                } else if path.len() == 2 {
                    &path[1].node
                } else {
                    return;
                };

                // Find variant's field types
                let field_types = info
                    .variant_field_types
                    .iter()
                    .find(|(name, _)| name == variant_name)
                    .map(|(_, types)| types.as_slice());

                let Some(field_types) = field_types else { return };

                for (i, field_pat) in fields.iter().enumerate() {
                    if let Some(spanned_ty) = field_types.get(i) {
                        let substituted = Self::substitute_generic_params(&spanned_ty.node, subst);
                        if let Some(tid) = self.ast_type_to_type_id(&substituted) {
                            self.register_pattern_binding_types(&field_pat.node, tid);
                        }
                    }
                }
            }
            Pattern::Or(alternatives) => {
                if let Some(first) = alternatives.first() {
                    self.register_pattern_var_types_inner(&first.node, info, subst);
                }
            }
            _ => {}
        }
    }

    /// Register a TypeId for a pattern binding (recursing into nested tuples).
    fn register_pattern_binding_types(
        &mut self,
        pattern: &Pattern,
        type_id: crate::semantic::ids::TypeId,
    ) {
        match pattern {
            Pattern::Binding(name) => {
                self.pattern_var_types.insert(name.clone(), type_id);
            }
            Pattern::Tuple(elements) => {
                if let crate::semantic::types::ResolvedType::Tuple(elem_tids) = self.types.get(type_id) {
                    let elem_tids = elem_tids.clone();
                    for (i, elem) in elements.iter().enumerate() {
                        if let Some(&tid) = elem_tids.get(i) {
                            self.register_pattern_binding_types(&elem.node, tid);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    /// Substitute generic type parameter names in an AST Type.
    /// E.g., `T` → `int` when `subst = {"T": int}`, or `Vector[T]` → `Vector[int]`.
    fn substitute_generic_params(
        ty: &Type,
        subst: &std::collections::HashMap<String, Type>,
    ) -> Type {
        match ty {
            Type::Named { name, generic_args } if generic_args.is_empty() => {
                // Bare name — might be a generic param like "T"
                if let Some(replacement) = subst.get(&name.node) {
                    return replacement.clone();
                }
                ty.clone()
            }
            Type::Named { name, generic_args } => {
                let new_args: Vec<_> = generic_args
                    .iter()
                    .map(|arg| {
                        crate::span::Spanned::new(
                            Self::substitute_generic_params(&arg.node, subst),
                            arg.span,
                        )
                    })
                    .collect();
                Type::Named {
                    name: name.clone(),
                    generic_args: new_args,
                }
            }
            Type::Tuple(types) => {
                let new_types: Vec<_> = types
                    .iter()
                    .map(|t| {
                        crate::span::Spanned::new(
                            Self::substitute_generic_params(&t.node, subst),
                            t.span,
                        )
                    })
                    .collect();
                Type::Tuple(new_types)
            }
            _ => ty.clone(),
        }
    }

    /// Return inline pattern bindings as a string (for use in GCC statement expressions).
    /// Uses statement-context enum variant lookup (vs expression-context in c_expr.rs).
    pub(super) fn stmt_pattern_bindings_inline(&mut self, pattern: &Pattern, scrutinee: &str) -> String {
        match pattern {
            Pattern::Binding(name) => {
                let escaped = c_mangle::escape_keyword(name);
                format!("__typeof__({scrutinee}) {escaped} = {scrutinee}; ")
            }
            Pattern::Constructor { path, fields } => {
                if let Some((_enum_name, variant_name)) = self.find_enum_for_variant_path(path) {
                    let mut result = String::new();
                    for (i, field_pat) in fields.iter().enumerate() {
                        let field_access =
                            format!("{scrutinee}.data.{variant_name}._{i}");
                        result.push_str(&self.stmt_pattern_bindings_inline(&field_pat.node, &field_access));
                    }
                    result
                } else {
                    String::new()
                }
            }
            Pattern::Tuple(elements) => {
                let mut result = String::new();
                for (i, elem) in elements.iter().enumerate() {
                    let field_access = format!("{scrutinee}._{i}");
                    result.push_str(&self.stmt_pattern_bindings_inline(&elem.node, &field_access));
                }
                result
            }
            Pattern::Or(alternatives) => {
                if let Some(first) = alternatives.first() {
                    self.stmt_pattern_bindings_inline(&first.node, scrutinee)
                } else {
                    String::new()
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => String::new(),
        }
    }

    /// Generate a block where `break` statements set a flag before breaking.
    /// Recurses into `if`, `match`, `unsafe`, and `with` bodies to find
    /// nested breaks, but stops at inner loops (`for`, `while`, `loop`)
    /// since those breaks target the inner loop.
    fn gen_block_with_break_flag(&mut self, block: &Block, flag: &str, emitter: &mut CEmitter) {
        for stmt in &block.stmts {
            self.gen_stmt_with_break_flag(&stmt.node, stmt.span, flag, emitter);
        }
    }

    /// Emit a single statement, instrumenting any `break` with the flag.
    fn gen_stmt_with_break_flag(&mut self, stmt: &Stmt, span: Span, flag: &str, emitter: &mut CEmitter) {
        match stmt {
            Stmt::Break(_) => {
                emitter.emit_line(&format!("{flag} = true;"));
                emitter.emit_line("break;");
            }
            // Recurse into if/elif/else — breaks inside target the outer loop
            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                let cond = self.gen_expr(condition);
                emitter.emit_line(&format!("if ({cond}) {{"));
                emitter.indent();
                self.push_drop_scope(DropScopeKind::Block);
                self.gen_block_with_break_flag(then_body, flag, emitter);
                self.pop_drop_scope(emitter);
                emitter.dedent();

                for (elif_cond, elif_body) in elif_branches {
                    let ec = self.gen_expr(elif_cond);
                    emitter.emit_line(&format!("}} else if ({ec}) {{"));
                    emitter.indent();
                    self.push_drop_scope(DropScopeKind::Block);
                    self.gen_block_with_break_flag(elif_body, flag, emitter);
                    self.pop_drop_scope(emitter);
                    emitter.dedent();
                }

                if let Some(else_body) = else_body {
                    emitter.emit_line("} else {");
                    emitter.indent();
                    self.push_drop_scope(DropScopeKind::Block);
                    self.gen_block_with_break_flag(else_body, flag, emitter);
                    self.pop_drop_scope(emitter);
                    emitter.dedent();
                }

                emitter.emit_line("}");
            }
            // Recurse into match arms
            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                let scrut_expr = self.gen_expr(scrutinee);
                let tmp = emitter.fresh_temp();
                emitter.emit_line(&format!(
                    "__typeof__({scrut_expr}) {tmp} = {scrut_expr};"
                ));
                let enum_c_type = self.resolve_enum_c_type_for_scrutinee(scrutinee);
                let mut first = true;
                for arm in arms {
                    let pattern_cond = self.pattern_to_condition(&arm.pattern.node, &tmp, enum_c_type.as_deref());
                    let full_cond = if let Some(guard) = &arm.guard {
                        let guard_expr = self.gen_expr(guard);
                        format!("({pattern_cond}) && ({guard_expr})")
                    } else {
                        pattern_cond
                    };
                    if first {
                        emitter.emit_line(&format!("if ({full_cond}) {{"));
                        first = false;
                    } else {
                        emitter.emit_line(&format!("}} else if ({full_cond}) {{"));
                    }
                    emitter.indent();
                    self.emit_pattern_bindings(&arm.pattern.node, &tmp, emitter);
                    // If arm body is a block, recurse into it for break flag handling
                    self.push_drop_scope(DropScopeKind::Block);
                    if let Expr::Block(block) = &arm.body.node {
                        self.gen_block_with_break_flag(block, flag, emitter);
                    } else {
                        let body = self.gen_expr(&arm.body);
                        emitter.emit_line(&format!("{body};"));
                    }
                    self.pop_drop_scope(emitter);
                    emitter.dedent();
                }
                if let Some(else_body) = else_arm {
                    emitter.emit_line("} else {");
                    emitter.indent();
                    self.push_drop_scope(DropScopeKind::Block);
                    self.gen_block_with_break_flag(else_body, flag, emitter);
                    self.pop_drop_scope(emitter);
                    emitter.dedent();
                }
                if !arms.is_empty() || else_arm.is_some() {
                    emitter.emit_line("}");
                }
            }
            // Recurse into unsafe/with blocks
            Stmt::Unsafe { body } => {
                emitter.emit_line("/* unsafe */ {");
                emitter.indent();
                self.push_drop_scope(DropScopeKind::Block);
                self.gen_block_with_break_flag(body, flag, emitter);
                self.pop_drop_scope(emitter);
                emitter.dedent();
                emitter.emit_line("}");
            }
            Stmt::With { bindings, body } => {
                emitter.emit_line("{");
                emitter.indent();
                for binding in bindings {
                    let e = self.gen_expr(&binding.expr);
                    let name = c_mangle::escape_keyword(&binding.name.node);
                    emitter.emit_line(&format!("/* with */ void* {name} = (void*)({e});"));
                }
                self.push_drop_scope(DropScopeKind::Block);
                self.gen_block_with_break_flag(body, flag, emitter);
                self.pop_drop_scope(emitter);
                emitter.dedent();
                emitter.emit_line("}");
            }
            // Do NOT recurse into inner loops — break inside them
            // targets the inner loop, not our outer for/else or while/else.
            _ => self.gen_stmt(stmt, span, emitter),
        }
    }

    /// Look up which enum owns a variant given a path (e.g., ["Some"] or ["Color", "Red"]).
    pub(super) fn find_enum_for_variant_path(&self, path: &[Spanned<String>]) -> Option<(String, String)> {
        let variant_name = if path.len() == 1 {
            &path[0].node
        } else if path.len() == 2 {
            // Path is [EnumName, VariantName]
            return Some((path[0].node.clone(), path[1].node.clone()));
        } else {
            return None;
        };

        // Search enum_variants to find which enum has this variant
        for (enum_def_id, info) in self.enum_variants {
            for (vname, _) in &info.variants {
                if vname == variant_name {
                    let enum_name = self.scopes.get_def(*enum_def_id).name.clone();
                    return Some((enum_name, variant_name.clone()));
                }
            }
        }
        None
    }

    /// Look up which enum owns a variant given a bare variant name.
    fn find_enum_for_variant_by_name(&self, variant_name: &str) -> Option<(String, String)> {
        for (enum_def_id, info) in self.enum_variants {
            for (vname, _) in &info.variants {
                if vname == variant_name {
                    let enum_name = self.scopes.get_def(*enum_def_id).name.clone();
                    return Some((enum_name, variant_name.to_string()));
                }
            }
        }
        None
    }
}

// ─── ExprVisitor-based collectors ──────────────────────────────

/// Scope-aware variable lookup: searches within the function scope tree when
/// a function scope is set, otherwise falls back to scope-chain walk.
/// Standalone helper for use by visitor structs that don't have access to
/// `CodegenContext`.
fn scoped_lookup_with(
    scopes: &ScopeTable,
    scope_id: Option<ScopeId>,
    name: &str,
) -> Option<crate::semantic::ids::DefId> {
    if let Some(sid) = scope_id {
        scopes.lookup_within_function(sid, name)
    } else {
        scopes.lookup(name)
    }
}

/// Collects free (unbound) variable names in a closure body without
/// requiring `&mut CodegenContext`.  Used to detect whether a closure
/// captures variables (for heap-allocation decisions).
struct FreeVarReadonlyCollector<'a> {
    bound: std::collections::HashSet<&'a str>,
    scopes: &'a ScopeTable,
    seen: std::collections::HashSet<String>,
    free: Vec<String>,
}

impl ExprVisitor for FreeVarReadonlyCollector<'_> {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Identifier(name) if !self.bound.contains(name.as_str()) && name != "self" => {
                if !self.scopes.is_global_def(name) && self.seen.insert(name.clone()) {
                    self.free.push(name.clone());
                }
            }
            // Don't recurse into nested closures
            Expr::Closure { .. } | Expr::ImplicitClosure { .. } => {}
            _ => visitor::walk_expr(self, expr),
        }
    }
}

/// Walks expressions collecting `Expr::Identifier` references suitable for
/// variable substitution in trace output.  Produces deduplicated
/// `(gorget_name, c_name, formatter_fn)` triples for traceable variables.
struct TraceVarCollector<'a> {
    scopes: &'a ScopeTable,
    types: &'a TypeTable,
    current_function_scope: Option<ScopeId>,
    seen: std::collections::HashSet<String>,
    out: Vec<(String, String, &'static str)>,
}

impl ExprVisitor for TraceVarCollector<'_> {
    fn visit_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Identifier(name) => {
                if self.seen.contains(name) {
                    return;
                }
                if let Some(def_id) = scoped_lookup_with(self.scopes, self.current_function_scope, name) {
                    let def = self.scopes.get_def(def_id);
                    match def.kind {
                        DefKind::Variable | DefKind::Const => {}
                        _ => return,
                    }
                    if let Some(type_id) = def.type_id {
                        let c_type = c_types::type_id_to_c(type_id, self.types, self.scopes);
                        if c_types::is_traceable_for_vars(&c_type) {
                            let formatter = c_types::trace_formatter_for_c_type(&c_type);
                            let c_name = c_mangle::escape_keyword(name);
                            self.seen.insert(name.clone());
                            self.out.push((name.clone(), c_name, formatter));
                        }
                    }
                }
            }
            // Skip callee position of calls — function names aren't trace vars
            Expr::Call { callee, args, .. } => {
                match &callee.node {
                    Expr::Identifier(_) | Expr::Path { .. } => {}
                    _ => self.visit_expr(callee),
                }
                for arg in args {
                    self.visit_expr(&arg.node.value);
                }
            }
            _ => visitor::walk_expr(self, expr),
        }
    }
}

/// Recursively collect all identifiers consumed by call arguments,
/// constructors, or enum variant constructors in an expression tree.
/// Used by the return handler to transfer ownership of all consumed variables.
fn collect_consumed_identifiers(expr: &Expr) -> Vec<String> {
    let mut result = Vec::new();
    collect_consumed_identifiers_inner(expr, &mut result);
    result
}

fn collect_consumed_identifiers_inner(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::Identifier(name) => {
            out.push(name.clone());
        }
        Expr::Call { callee, args, .. } => {
            // Recurse into the callee expr (for enum variant names)
            collect_consumed_identifiers_inner(&callee.node, out);
            // Recurse into all arguments
            for a in args {
                collect_consumed_identifiers_inner(&a.node.value.node, out);
            }
        }
        Expr::StructLiteral { args, .. } => {
            for a in args {
                collect_consumed_identifiers_inner(&a.node, out);
            }
        }
        Expr::MethodCall { args, .. } => {
            // Recurse into args but NOT receiver (receiver isn't consumed)
            for a in args {
                collect_consumed_identifiers_inner(&a.node.value.node, out);
            }
        }
        Expr::If { then_branch, else_branch, .. } => {
            // Branches are Spanned<Expr> — recurse directly
            collect_consumed_identifiers_inner(&then_branch.node, out);
            if let Some(else_b) = else_branch {
                collect_consumed_identifiers_inner(&else_b.node, out);
            }
        }
        Expr::Match { arms, else_arm, .. } => {
            for arm in arms {
                collect_consumed_identifiers_inner(&arm.body.node, out);
            }
            if let Some(else_e) = else_arm {
                collect_consumed_identifiers_inner(&else_e.node, out);
            }
        }
        Expr::Move { expr } | Expr::Try { expr } => {
            collect_consumed_identifiers_inner(&expr.node, out);
        }
        Expr::Block(block) => {
            if let Some(last) = block.stmts.last() {
                if let Stmt::Expr(e) = &last.node {
                    collect_consumed_identifiers_inner(&e.node, out);
                } else if let Stmt::Return(Some(e)) = &last.node {
                    collect_consumed_identifiers_inner(&e.node, out);
                }
            }
        }
        Expr::Do { body } => {
            if let Some(last) = body.stmts.last() {
                if let Stmt::Expr(e) = &last.node {
                    collect_consumed_identifiers_inner(&e.node, out);
                } else if let Stmt::Return(Some(e)) = &last.node {
                    collect_consumed_identifiers_inner(&e.node, out);
                }
            }
        }
        // Other expression types (literals, binary ops, etc.) don't consume identifiers
        _ => {}
    }
}

/// Convert a compound assignment operator to its C form.
fn compound_op_to_c(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add | BinaryOp::AddWrap => "+=",
        BinaryOp::Sub | BinaryOp::SubWrap => "-=",
        BinaryOp::Mul | BinaryOp::MulWrap => "*=",
        BinaryOp::Div => "/=",
        BinaryOp::Mod => "%=",
        BinaryOp::BitAnd => "&=",
        BinaryOp::BitOr => "|=",
        BinaryOp::BitXor => "^=",
        BinaryOp::Shl => "<<=",
        BinaryOp::Shr => ">>=",
        _ => "/* ?? */=",
    }
}
