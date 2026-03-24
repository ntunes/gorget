use std::sync::atomic::{AtomicUsize, Ordering};

use crate::parser::ast::{
    Attribute, AttributeArg, EnumDef, GenericParam, GenericParams, Item, Module, StructDef,
    VariantFields,
};
use crate::parser::Parser;
use crate::span::Spanned;
// Span is used by tests (Span::dummy())
#[cfg(test)]
use crate::span::Span;

use super::errors::{SemanticError, SemanticErrorKind};
use crate::span::Span as SpanType;

/// Metadata about a derive expansion, used for post-Pass-3 field trait validation.
pub struct DeriveRecord {
    pub struct_name: String,
    pub trait_name: String,
    pub field_type_names: Vec<String>,
    pub span: SpanType,
}

/// Global counter for allocating unique span offsets for derive-generated items.
/// Each derive source string gets a unique base offset (spaced 100K apart) to prevent
/// span collisions between different derive-generated equip blocks/functions.
static DERIVE_SPAN_OFFSET: AtomicUsize = AtomicUsize::new(10_000_000);

/// Format generic parameters back to source text.
/// Returns `""` for `None`, `"[T]"` for single param, `"[T, U]"` for multiple.
fn format_generic_params(gp: &Option<Spanned<GenericParams>>) -> String {
    match gp {
        Some(gp) => {
            let params: Vec<&str> = gp
                .node
                .params
                .iter()
                .map(|p| match &p.node {
                    GenericParam::Type { name, .. } => name.node.as_str(),
                    _ => "auto",
                })
                .collect();
            format!("[{}]", params.join(", "))
        }
        None => String::new(),
    }
}

/// Expand all `@derive(...)` attributes on structs and enums in the module.
///
/// For each derivable trait, generates Gorget source code for the equip block,
/// parses it, and appends the resulting `EquipBlock` items to the module.
/// Recurses into `Item::Module` wrappers so derives in imported modules are expanded.
pub fn expand_derives(module: &mut Module, errors: &mut Vec<SemanticError>) -> Vec<DeriveRecord> {
    let mut records = Vec::new();
    expand_derives_in_items(&mut module.items, errors, &mut records);
    records
}

fn expand_derives_in_items(items: &mut Vec<Spanned<Item>>, errors: &mut Vec<SemanticError>, records: &mut Vec<DeriveRecord>) {
    // First, recurse into Item::Module wrappers so nested modules get expanded too.
    for item in items.iter_mut() {
        if let Item::Module { items: inner, .. } = &mut item.node {
            expand_derives_in_items(inner, errors, records);
        }
    }

    // Then collect derives from structs/enums at this level.
    let mut new_items = Vec::new();
    for item in items.iter() {
        match &item.node {
            Item::Struct(s) => {
                collect_struct_derives(s, &mut new_items, errors, records);
            }
            Item::Enum(e) => {
                collect_enum_derives(e, &mut new_items, errors);
            }
            _ => {}
        }
    }

    items.extend(new_items);
}

const DERIVABLE_STRUCT_TRAITS: &[&str] =
    &["Equatable", "Displayable", "Cloneable", "Hashable", "Serializable", "Default", "Deserializable", "From", "TryFrom", "FromRow"];
const DERIVABLE_ENUM_TRAITS: &[&str] =
    &["Equatable", "Displayable", "Cloneable", "Hashable", "Serializable", "Deserializable", "Ordinal"];

fn collect_struct_derives(
    s: &StructDef,
    new_items: &mut Vec<Spanned<Item>>,
    errors: &mut Vec<SemanticError>,
    records: &mut Vec<DeriveRecord>,
) {
    let type_name = &s.name.node;
    let gs = format_generic_params(&s.generic_params);

    for trait_name in extract_derive_args(&s.attributes) {
        if !DERIVABLE_STRUCT_TRAITS.contains(&trait_name.as_str()) {
            errors.push(SemanticError {
                kind: SemanticErrorKind::UnderivableTrait {
                    trait_name: trait_name.clone(),
                    type_name: type_name.clone(),
                },
                span: s.span,
            });
            continue;
        }

        if (trait_name == "From" || trait_name == "TryFrom") && s.fields.len() != 1 {
            errors.push(SemanticError {
                kind: SemanticErrorKind::DeriveFromRequiresSingleField {
                    type_name: type_name.clone(),
                },
                span: s.span,
            });
            continue;
        }

        let fields: Vec<(&str, &str)> = s
            .fields
            .iter()
            .map(|f| {
                let name = f.node.name.node.as_str();
                let ty = format_type(&f.node.type_.node);
                // We leak the string so we get a &str with 'static lifetime.
                // This is fine — derive expansion happens once at startup.
                let ty: &str = Box::leak(ty.into_boxed_str());
                (name, ty)
            })
            .collect();

        // Record derive metadata for post-Pass-3 field trait validation.
        records.push(DeriveRecord {
            struct_name: type_name.clone(),
            trait_name: trait_name.clone(),
            field_type_names: fields.iter().map(|(_, ty)| ty.to_string()).collect(),
            span: s.span,
        });

        let source = generate_struct_derive(type_name, &gs, &trait_name, &fields);
        parse_and_collect_derived_items(&source, new_items);
    }
}

/// Traits that require all fields to implement the same trait.
const FIELD_REQUIRING_TRAITS: &[&str] = &["Hashable", "Equatable", "Cloneable"];

/// Primitive types that satisfy common traits implicitly.
fn primitive_satisfies(type_name: &str, trait_name: &str) -> bool {
    match trait_name {
        "Equatable" | "Cloneable" | "Displayable" => {
            // All primitives satisfy these
            matches!(type_name, "int" | "int8" | "int16" | "int32" | "int64"
                | "uint" | "uint8" | "uint16" | "uint32" | "uint64"
                | "float" | "float32" | "float64" | "bool" | "String")
        }
        "Hashable" => {
            // Floats are NOT hashable
            matches!(type_name, "int" | "int8" | "int16" | "int32" | "int64"
                | "uint" | "uint8" | "uint16" | "uint32" | "uint64"
                | "bool" | "String")
        }
        _ => false,
    }
}

/// Post-Pass-3 validation: check that @derive'd traits have field types
/// that implement the required trait.
pub fn validate_derive_field_traits(
    records: &[DeriveRecord],
    traits: &super::traits::TraitRegistry,
    errors: &mut Vec<SemanticError>,
) {
    for record in records {
        if !FIELD_REQUIRING_TRAITS.contains(&record.trait_name.as_str()) {
            continue;
        }
        for field_type in &record.field_type_names {
            // Skip generic type parameters (single uppercase letter or generic names)
            if field_type.len() <= 2 && field_type.chars().next().map_or(false, |c| c.is_uppercase()) {
                continue;
            }
            // Skip collection/generic types like Vector[T], Option[T] — they have
            // their own derive or are handled by the serialization framework.
            if field_type.contains('[') {
                continue;
            }
            // Check primitives
            if primitive_satisfies(field_type, &record.trait_name) {
                continue;
            }
            // Check trait registry for user-defined types
            if traits.has_trait_impl_by_name(field_type, &record.trait_name) {
                continue;
            }
            // Check if the field type itself has a derive record for this trait
            // (handles cross-module derives where the library module's equip blocks
            // aren't yet in the trait registry)
            let derived_elsewhere = records.iter().any(|r| {
                r.struct_name == *field_type && r.trait_name == record.trait_name
            });
            if derived_elsewhere {
                continue;
            }
            // Field type doesn't implement the required trait
            errors.push(SemanticError {
                kind: SemanticErrorKind::FieldMissingDerivedTrait {
                    struct_name: record.struct_name.clone(),
                    field_type: field_type.clone(),
                    trait_name: record.trait_name.clone(),
                },
                span: record.span,
            });
        }
    }
}

fn collect_enum_derives(
    e: &EnumDef,
    new_items: &mut Vec<Spanned<Item>>,
    errors: &mut Vec<SemanticError>,
) {
    let type_name = &e.name.node;
    let gs = format_generic_params(&e.generic_params);

    for trait_name in extract_derive_args(&e.attributes) {
        if !DERIVABLE_ENUM_TRAITS.contains(&trait_name.as_str()) {
            errors.push(SemanticError {
                kind: SemanticErrorKind::UnderivableTrait {
                    trait_name: trait_name.clone(),
                    type_name: type_name.clone(),
                },
                span: e.span,
            });
            continue;
        }

        let source = generate_enum_derive(type_name, &gs, &trait_name, e);
        parse_and_collect_derived_items(&source, new_items);
    }
}

/// Extract trait names from `@derive(Trait1, Trait2, ...)` attributes.
fn extract_derive_args(attributes: &[Spanned<Attribute>]) -> Vec<String> {
    let mut result = Vec::new();
    for attr in attributes {
        if attr.node.name.node == "derive" {
            for arg in &attr.node.args {
                if let AttributeArg::Identifier(name) = arg {
                    result.push(name.clone());
                }
            }
        }
    }
    result
}

/// Format a type AST node back to source text (for simple types).
fn format_type(ty: &crate::parser::ast::Type) -> String {
    use crate::parser::ast::{PrimitiveType, Type};
    match ty {
        Type::Primitive(p) => match p {
            PrimitiveType::Int => "int".to_string(),
            PrimitiveType::Int8 => "int8".to_string(),
            PrimitiveType::Int16 => "int16".to_string(),
            PrimitiveType::Int32 => "int32".to_string(),
            PrimitiveType::Int64 => "int64".to_string(),
            PrimitiveType::Uint => "uint".to_string(),
            PrimitiveType::Uint8 => "uint8".to_string(),
            PrimitiveType::Uint16 => "uint16".to_string(),
            PrimitiveType::Uint32 => "uint32".to_string(),
            PrimitiveType::Uint64 => "uint64".to_string(),
            PrimitiveType::Float => "float".to_string(),
            PrimitiveType::Float32 => "float32".to_string(),
            PrimitiveType::Float64 => "float64".to_string(),
            PrimitiveType::Bool => "bool".to_string(),
            PrimitiveType::StringView => "String".to_string(),
            PrimitiveType::CStr => "cstr".to_string(),
            PrimitiveType::StringType => "String".to_string(),
            PrimitiveType::Void => "void".to_string(),
        },
        Type::Named { name, generic_args } => {
            if generic_args.is_empty() {
                name.node.clone()
            } else {
                let args: Vec<String> = generic_args.iter().map(|a| format_type(&a.node)).collect();
                format!("{}[{}]", name.node, args.join(", "))
            }
        }
        _ => "auto".to_string(),
    }
}

/// Returns `""` for non-generic types, `"[T] "` (with trailing space) for generic.
/// Used as the equip block generic-param prefix: `equip {gp}{type_name}...`
fn equip_generic_prefix(gs: &str) -> String {
    if gs.is_empty() {
        String::new()
    } else {
        format!("{gs} ")
    }
}

// ── Struct derive generation ──────────────────────────────────

fn generate_struct_derive(
    type_name: &str,
    gs: &str,
    trait_name: &str,
    fields: &[(&str, &str)],
) -> String {
    match trait_name {
        "Equatable" => generate_struct_equatable(type_name, gs, fields),
        "Displayable" => generate_struct_displayable(type_name, gs, fields),
        "Cloneable" => generate_struct_cloneable(type_name, gs, fields),
        "Hashable" => generate_struct_hashable(type_name, gs, fields),
        "Serializable" => generate_struct_serializable(type_name, gs, fields),
        "Default" => generate_struct_default(type_name, gs, fields),
        "Deserializable" => generate_struct_deserializable(type_name, gs, fields),
        "From" => generate_struct_from(type_name, gs, fields),
        "TryFrom" => generate_struct_try_from(type_name, gs, fields),
        "FromRow" => generate_struct_from_row(type_name, gs, fields),
        _ => String::new(),
    }
}

fn generate_struct_equatable(type_name: &str, gs: &str, fields: &[(&str, &str)]) -> String {
    let gp = equip_generic_prefix(gs);
    let body = if fields.is_empty() {
        "        return true".to_string()
    } else {
        let comparisons: Vec<String> = fields
            .iter()
            .map(|(name, _)| format!("self.{name} == other.{name}"))
            .collect();
        format!("        return {}", comparisons.join(" and "))
    };

    format!(
        "equip {gp}{type_name}{gs} with Equatable:\n    bool eq(self, {type_name}{gs} other):\n{body}\n"
    )
}

fn generate_struct_displayable(type_name: &str, gs: &str, fields: &[(&str, &str)]) -> String {
    let gp = equip_generic_prefix(gs);
    let body = if fields.is_empty() {
        format!("        return \"{type_name}()\"")
    } else {
        let parts: Vec<String> = fields
            .iter()
            .map(|(name, _)| format!("{name}={{self.{name}}}"))
            .collect();
        format!(
            "        return f\"{type_name}({})\"",
            parts.join(", ")
        )
    };

    format!(
        "equip {gp}{type_name}{gs} with Displayable:\n    String display(self):\n{body}\n"
    )
}

fn generate_struct_cloneable(type_name: &str, gs: &str, fields: &[(&str, &str)]) -> String {
    let gp = equip_generic_prefix(gs);
    let args: Vec<String> = fields.iter().map(|(name, _)| format!("self.{name}")).collect();
    let body = format!("        return {type_name}{gs}({})", args.join(", "));

    format!(
        "equip {gp}{type_name}{gs} with Cloneable:\n    {type_name}{gs} clone(self):\n{body}\n"
    )
}

fn generate_struct_hashable(type_name: &str, gs: &str, fields: &[(&str, &str)]) -> String {
    let body = if fields.is_empty() {
        "        return 0".to_string()
    } else if fields.len() == 1 {
        format!("        return self.{}.hash()", fields[0].0)
    } else {
        let mut lines = vec![format!(
            "        int h = self.{}.hash()",
            fields[0].0
        )];
        for (name, _) in &fields[1..] {
            lines.push(format!("        h = h *% 31 +% self.{name}.hash()"));
        }
        lines.push("        return h".to_string());
        lines.join("\n")
    };

    let gp = equip_generic_prefix(gs);
    format!(
        "equip {gp}{type_name}{gs} with Hashable:\n    int hash(self):\n{body}\n"
    )
}

fn generate_struct_serializable(type_name: &str, gs: &str, fields: &[(&str, &str)]) -> String {
    let gp = equip_generic_prefix(gs);
    let mut body = String::new();
    body.push_str(&format!(
        "        ser.begin_struct(\"{type_name}\", {})\n",
        fields.len()
    ));
    for (i, (name, ty)) in fields.iter().enumerate() {
        body.push_str(&format!("        ser.field(\"{name}\", {i})\n"));
        body.push_str(&field_write_lines(
            &format!("self.{name}"),
            ty,
            "        ",
        ));
    }
    body.push_str("        ser.end_struct()");

    format!(
        "equip {gp}{type_name}{gs} with Serializable:\n\
         \x20   void serialize(self, Box[Serializer] ser):\n\
         {body}\n"
    )
}

// ── Collection type detection ─────────────────────────────────

/// Recognized collection wrapper types for special-cased derive codegen.
enum CollectionKind {
    Vector(String),        // Vector[T] → element type
    Option(String),        // Option[T] → inner type
    Dict(String, String),  // Dict[K, V] → key, value types
    HashMap(String, String), // HashMap[K, V] → key, value types
}

/// Parse a type string to detect Vector, Option, Dict, or HashMap wrappers.
fn parse_collection_type(ty: &str) -> Option<CollectionKind> {
    if let Some(inner) = strip_generic(ty, "Vector") {
        Some(CollectionKind::Vector(inner.to_string()))
    } else if let Some(inner) = strip_generic(ty, "Option") {
        Some(CollectionKind::Option(inner.to_string()))
    } else if let Some(inner) = strip_generic(ty, "Dict") {
        let (k, v) = split_top_level_comma(inner)?;
        Some(CollectionKind::Dict(k.trim().to_string(), v.trim().to_string()))
    } else if let Some(inner) = strip_generic(ty, "HashMap") {
        let (k, v) = split_top_level_comma(inner)?;
        Some(CollectionKind::HashMap(k.trim().to_string(), v.trim().to_string()))
    } else {
        None
    }
}

/// Strip `Name[...]` wrapper, returning the content between brackets.
/// Handles nested brackets: `Vector[Dict[String, int]]` → `Dict[String, int]`.
fn strip_generic<'a>(ty: &'a str, name: &str) -> Option<&'a str> {
    let rest = ty.strip_prefix(name)?;
    let rest = rest.strip_prefix('[')?;
    let rest = rest.strip_suffix(']')?;
    Some(rest)
}

/// Split a string by the first top-level comma (respecting bracket nesting).
fn split_top_level_comma(s: &str) -> Option<(&str, &str)> {
    let mut depth = 0;
    for (i, c) in s.char_indices() {
        match c {
            '[' => depth += 1,
            ']' => depth -= 1,
            ',' if depth == 0 => {
                return Some((&s[..i], &s[i + 1..]));
            }
            _ => {}
        }
    }
    None
}

/// Global counter for allocating unique temporary variable names across all
/// derive-generated code. Ensures unique names across different equip blocks.
static DERIVE_VAR_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Generate a unique temporary variable name (globally unique across all derives).
fn next_var() -> String {
    let v = DERIVE_VAR_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("__t{v}")
}

// ── Multi-line serialization codegen ──────────────────────────

/// Generate serialization code for writing `expr` of type `type_name`.
/// Returns properly indented multi-line code. Recurses for nested collections.
fn field_write_lines(expr: &str, type_name: &str, indent: &str) -> String {
    match parse_collection_type(type_name) {
        Some(CollectionKind::Vector(ref elem_ty)) => {
            let idx = next_var();
            let val = next_var();
            let inner = format!("{indent}    ");
            let elem_lines = field_write_lines(&val, elem_ty, &inner);
            format!(
                "{indent}ser.begin_seq({expr}.len())\n\
                 {indent}int {idx} = 0\n\
                 {indent}while {idx} < {expr}.len():\n\
                 {inner}ser.elem({idx})\n\
                 {inner}{elem_ty} {val} = {expr}.get({idx}).unwrap()\n\
                 {elem_lines}\
                 {inner}{idx} = {idx} + 1\n\
                 {indent}ser.end_seq()\n"
            )
        }
        Some(CollectionKind::Option(ref inner_ty)) => {
            let val = next_var();
            let case_indent = format!("{indent}    ");
            let body_indent = format!("{indent}        ");
            let write_lines = field_write_lines(&val, inner_ty, &body_indent);
            format!(
                "{indent}match {expr}:\n\
                 {case_indent}case Some({val}):\n\
                 {write_lines}\
                 {case_indent}case None:\n\
                 {body_indent}ser.write_null()\n"
            )
        }
        Some(CollectionKind::Dict(ref key_ty, ref val_ty))
        | Some(CollectionKind::HashMap(ref key_ty, ref val_ty))
            if key_ty == "str" || key_ty == "String" =>
        {
            let keys_var = next_var();
            let idx = next_var();
            let key_var = next_var();
            let val_var = next_var();
            let inner = format!("{indent}    ");
            let val_lines = field_write_lines(&val_var, val_ty, &inner);
            format!(
                "{indent}Vector[String] {keys_var} = {expr}.keys()\n\
                 {indent}ser.begin_struct(\"\", {keys_var}.len())\n\
                 {indent}int {idx} = 0\n\
                 {indent}while {idx} < {keys_var}.len():\n\
                 {inner}String {key_var} = {keys_var}.get({idx}).unwrap()\n\
                 {inner}ser.field({key_var}, {idx})\n\
                 {inner}{val_ty} {val_var} = {expr}[{key_var}]\n\
                 {val_lines}\
                 {inner}{idx} = {idx} + 1\n\
                 {indent}ser.end_struct()\n"
            )
        }
        _ => {
            // Primitive or named type — single line
            format!("{indent}{}\n", field_write_call(expr, type_name))
        }
    }
}

// ── Multi-line deserialization codegen ─────────────────────────

/// Generate deserialization code that declares variable `name` of type `type_name`.
/// Returns properly indented multi-line code. Recurses for nested collections.
fn field_read_lines(name: &str, type_name: &str, indent: &str) -> String {
    match parse_collection_type(type_name) {
        Some(CollectionKind::Vector(ref elem_ty)) => {
            let len_var = next_var();
            let idx = next_var();
            let elem_var = next_var();
            let inner = format!("{indent}    ");
            let elem_lines = field_read_lines(&elem_var, elem_ty, &inner);
            format!(
                "{indent}int {len_var} = de.begin_seq()\n\
                 {indent}{type_name} {name} = {type_name}()\n\
                 {indent}int {idx} = 0\n\
                 {indent}while {idx} < {len_var}:\n\
                 {inner}de.elem({idx})\n\
                 {elem_lines}\
                 {inner}{name}.push({elem_var})\n\
                 {inner}{idx} = {idx} + 1\n\
                 {indent}de.end_seq()\n"
            )
        }
        Some(CollectionKind::Option(ref inner_ty)) => {
            let val_var = next_var();
            let inner = format!("{indent}    ");
            let val_lines = field_read_lines(&val_var, inner_ty, &inner);
            format!(
                "{indent}{type_name} {name} = None()\n\
                 {indent}if not de.is_null():\n\
                 {val_lines}\
                 {inner}{name} = Some({val_var})\n"
            )
        }
        Some(CollectionKind::Dict(ref key_ty, ref val_ty))
        | Some(CollectionKind::HashMap(ref key_ty, ref val_ty))
            if key_ty == "str" || key_ty == "String" =>
        {
            let keys_var = next_var();
            let idx = next_var();
            let key_var = next_var();
            let val_var = next_var();
            let inner = format!("{indent}    ");
            let val_lines = field_read_lines(&val_var, val_ty, &inner);
            format!(
                "{indent}de.begin_struct()\n\
                 {indent}Vector[String] {keys_var} = de.keys()\n\
                 {indent}{type_name} {name} = {type_name}()\n\
                 {indent}int {idx} = 0\n\
                 {indent}while {idx} < {keys_var}.len():\n\
                 {inner}String {key_var} = {keys_var}.get({idx}).unwrap()\n\
                 {inner}de.field({key_var})\n\
                 {val_lines}\
                 {inner}{name}.put({key_var}, {val_var})\n\
                 {inner}{idx} = {idx} + 1\n\
                 {indent}de.end_struct()\n"
            )
        }
        _ => {
            // Primitive or named type — single line
            format!("{indent}{}\n", field_read_decl(name, type_name))
        }
    }
}

/// Generate the serializer call for a single field expression based on its type.
fn field_write_call(expr: &str, type_name: &str) -> String {
    match type_name {
        "int" => format!("ser.write_int({expr})"),
        "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16" | "uint32"
        | "uint64" => {
            format!("ser.write_int({expr} as int)")
        }
        "float" => format!("ser.write_float({expr})"),
        "float32" | "float64" => format!("ser.write_float({expr} as float)"),
        "bool" => format!("ser.write_bool({expr})"),
        "String" => format!("ser.write_str({expr})"),
        _ => format!("{expr}.serialize(ser)"),
    }
}

fn generate_struct_default(type_name: &str, gs: &str, fields: &[(&str, &str)]) -> String {
    let gp = equip_generic_prefix(gs);
    let defaults: Vec<String> = fields.iter().map(|(_, ty)| field_default_value(ty)).collect();
    let body = format!("        return {type_name}{gs}({})", defaults.join(", "));

    format!(
        "equip {gp}{type_name}{gs} with Default:\n    {type_name}{gs} default():\n{body}\n"
    )
}

/// Generate the default value expression for a field type.
fn field_default_value(type_name: &str) -> String {
    match type_name {
        "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16" | "uint32"
        | "uint64" => "0".to_string(),
        "float" | "float32" | "float64" => "0.0".to_string(),
        "bool" => "false".to_string(),
        "String" => "String()".to_string(),
        other => format!("{other}.default()"),
    }
}


fn generate_struct_from(type_name: &str, gs: &str, fields: &[(&str, &str)]) -> String {
    let (_, field_type) = &fields[0];
    let gp = equip_generic_prefix(gs);
    format!(
        "equip {gp}{type_name}{gs} with From[{field_type}]:\n    \
         {type_name}{gs} from({field_type} value):\n        \
         return {type_name}{gs}(value)\n"
    )
}

fn generate_struct_try_from(type_name: &str, gs: &str, fields: &[(&str, &str)]) -> String {
    let (_, field_type) = &fields[0];
    let gp = equip_generic_prefix(gs);
    format!(
        "equip {gp}{type_name}{gs} with TryFrom[{field_type}]:\n    \
         Result[{type_name}{gs}, String] try_from({field_type} value):\n        \
         return Ok({type_name}{gs}(value))\n"
    )
}

/// `@derive(FromRow)` — auto-generate `equip T with FromRow` for structs.
///
/// Maps column names to field names using `Row.get()`, `Row.get_int()`,
/// `Row.get_float()`, `Row.get_bool()`. The trait method signature is
/// `Self from_row(Row row)`. Only String, int, float, bool fields are handled
/// natively; all other field types receive the raw column text (String).
fn generate_struct_from_row(type_name: &str, gs: &str, fields: &[(&str, &str)]) -> String {
    let gp = equip_generic_prefix(gs);
    let mut body = String::new();
    for (name, ty) in fields {
        match *ty {
            "String" => {
                body.push_str(&format!("        String {name} = row.get(\"{name}\")\n"));
            }
            "int" | "int8" | "int16" | "int32" | "int64"
            | "uint" | "uint8" | "uint16" | "uint32" | "uint64" => {
                body.push_str(&format!("        int {name} = row.get_int(\"{name}\")\n"));
            }
            "float" | "float32" | "float64" => {
                body.push_str(&format!("        float {name} = row.get_float(\"{name}\")\n"));
            }
            "bool" => {
                body.push_str(&format!("        bool {name} = row.get_bool(\"{name}\")\n"));
            }
            _ => {
                // Fall back to raw string for unknown types
                body.push_str(&format!("        String {name} = row.get(\"{name}\")\n"));
            }
        }
    }
    let args: Vec<&str> = fields.iter().map(|(name, _)| *name).collect();
    body.push_str(&format!(
        "        return {type_name}{gs}({})\n",
        args.join(", ")
    ));

    format!(
        "equip {gp}{type_name}{gs} with FromRow:\n    {type_name}{gs} from_row(Row row):\n{body}"
    )
}

fn generate_struct_deserializable(type_name: &str, gs: &str, fields: &[(&str, &str)]) -> String {
    let mut body = String::new();
    body.push_str("    de.begin_struct()\n");
    for (name, ty) in fields {
        body.push_str(&format!("    de.field(\"{name}\")\n"));
        body.push_str(&field_read_lines(name, ty, "    "));
    }
    body.push_str("    de.end_struct()\n");
    body.push_str("    if de.has_error():\n");
    body.push_str("        return Error(de.error())\n");
    let args: Vec<&str> = fields.iter().map(|(name, _)| *name).collect();
    body.push_str(&format!(
        "    return Ok({type_name}{gs}({}))\n",
        args.join(", ")
    ));

    format!(
        "Result[{type_name}{gs}, String] deserialize_{type_name}(Box[Deserializer] de):\n{body}"
    )
}

/// Generate the deserializer read declaration for a single field based on its type.
fn field_read_decl(name: &str, type_name: &str) -> String {
    match type_name {
        "int" => format!("int {name} = de.read_int()"),
        "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16" | "uint32"
        | "uint64" => {
            format!(
                "int {name}_raw = de.read_int()\n    {type_name} {name} = {name}_raw as {type_name}"
            )
        }
        "float" => format!("float {name} = de.read_float()"),
        "float32" | "float64" => {
            format!(
                "float {name}_raw = de.read_float()\n    {type_name} {name} = {name}_raw as {type_name}"
            )
        }
        "bool" => format!("bool {name} = de.read_bool()"),
        "String" => format!("String {name} = de.read_str()"),
        _ => format!("{type_name} {name} = deserialize_{type_name}(de)"),
    }
}

fn generate_enum_deserializable(type_name: &str, gs: &str, e: &EnumDef) -> String {
    let pfx = variant_prefix(type_name, e);
    // Separate unit variants from data variants
    let mut unit_variants = Vec::new();
    let mut data_variants = Vec::new();

    for variant in &e.variants {
        let vname = &variant.node.name.node;
        match &variant.node.fields {
            VariantFields::Unit => unit_variants.push(vname.as_str()),
            VariantFields::Tuple(fields) => {
                let field_types: Vec<&str> = fields
                    .iter()
                    .map(|f| {
                        let ty = format_type(&f.node);
                        Box::leak(ty.into_boxed_str()) as &str
                    })
                    .collect();
                data_variants.push((vname.as_str(), field_types));
            }
        }
    }

    let mut body = String::new();

    // Handle unit variants via is_string() check
    if !unit_variants.is_empty() {
        body.push_str("    if de.is_string():\n");
        body.push_str("        String de_tag = de.read_str()\n");
        for (i, vname) in unit_variants.iter().enumerate() {
            if i == 0 {
                body.push_str(&format!("        if de_tag == \"{vname}\":\n"));
            } else {
                body.push_str(&format!("        elif de_tag == \"{vname}\":\n"));
            }
            body.push_str(&format!("            return Ok({pfx}{vname}())\n"));
        }
        body.push_str("        return Error(\"unknown variant\")\n");
    }

    // Handle data variants via is_struct() + has_field()
    if !data_variants.is_empty() {
        body.push_str("    de.begin_struct()\n");
        for (i, (vname, field_types)) in data_variants.iter().enumerate() {
            let cond = if i == 0 { "if" } else { "elif" };
            body.push_str(&format!("    {cond} de.has_field(\"{vname}\"):\n"));
            body.push_str(&format!("        de.field(\"{vname}\")\n"));
            body.push_str("        int de_seq_len = de.begin_seq()\n");
            for (fi, fty) in field_types.iter().enumerate() {
                body.push_str(&format!("        de.elem({fi})\n"));
                let var_name = format!("a{fi}");
                body.push_str(&field_read_lines(&var_name, fty, "        "));
            }
            body.push_str("        de.end_seq()\n");
            body.push_str("        de.end_struct()\n");
            body.push_str("        if de.has_error():\n");
            body.push_str("            return Error(de.error())\n");
            let args: Vec<String> =
                (0..field_types.len()).map(|i| format!("a{i}")).collect();
            body.push_str(&format!(
                "        return Ok({pfx}{vname}({}))\n",
                args.join(", ")
            ));
        }
        body.push_str("    de.end_struct()\n");
    }

    body.push_str("    return Error(\"unknown variant\")\n");

    format!(
        "Result[{type_name}{gs}, String] deserialize_{type_name}(Box[Deserializer] de):\n{body}"
    )
}

// ── Enum derive generation ────────────────────────────────────

/// Returns the prefix to use before a variant name in patterns/constructors.
/// Non-generic enums: `"Color."` (qualified access required).
/// Generic enums: `""` (variants stay in scope as bare names).
fn variant_prefix<'a>(type_name: &'a str, e: &EnumDef) -> std::borrow::Cow<'a, str> {
    if e.generic_params.is_some() {
        std::borrow::Cow::Borrowed("")
    } else {
        std::borrow::Cow::Owned(format!("{type_name}."))
    }
}

fn generate_enum_derive(type_name: &str, gs: &str, trait_name: &str, e: &EnumDef) -> String {
    match trait_name {
        "Equatable" => generate_enum_equatable(type_name, gs, e),
        "Displayable" => generate_enum_displayable(type_name, gs, e),
        "Cloneable" => generate_enum_cloneable(type_name, gs, e),
        "Hashable" => generate_enum_hashable(type_name, gs, e),
        "Serializable" => generate_enum_serializable(type_name, gs, e),
        "Deserializable" => generate_enum_deserializable(type_name, gs, e),
        "Ordinal" => generate_enum_ordinal(type_name, gs, e),
        _ => String::new(),
    }
}

fn generate_enum_ordinal(type_name: &str, gs: &str, e: &EnumDef) -> String {
    let gp = equip_generic_prefix(gs);
    let pfx = variant_prefix(type_name, e);
    let mut arms = String::new();

    for (idx, variant) in e.variants.iter().enumerate() {
        let vname = &variant.node.name.node;
        let field_count = match &variant.node.fields {
            VariantFields::Unit => 0,
            VariantFields::Tuple(fields) => fields.len(),
        };

        let pattern = if field_count == 0 {
            format!("{pfx}{vname}()")
        } else {
            let bindings: Vec<String> = (0..field_count).map(|i| format!("a{i}")).collect();
            format!("{pfx}{vname}({})", bindings.join(", "))
        };

        arms.push_str(&format!(
            "            case {pattern}:\n\
             \x20               return {idx}\n"
        ));
    }

    format!(
        "equip {gp}{type_name}{gs} with Ordinal:\n\
         \x20   int ordinal(self):\n\
         \x20       match self:\n\
         {arms}"
    )
}

fn generate_enum_equatable(type_name: &str, gs: &str, e: &EnumDef) -> String {
    let gp = equip_generic_prefix(gs);
    let pfx = variant_prefix(type_name, e);
    let mut arms = String::new();

    for variant in &e.variants {
        let vname = &variant.node.name.node;
        let field_count = match &variant.node.fields {
            VariantFields::Unit => 0,
            VariantFields::Tuple(fields) => fields.len(),
        };

        // Generate binding names for self: a0, a1, ...
        let self_bindings: Vec<String> = (0..field_count).map(|i| format!("a{i}")).collect();
        // Generate binding names for other: b0, b1, ...
        let other_bindings: Vec<String> = (0..field_count).map(|i| format!("b{i}")).collect();

        let self_pattern = if field_count == 0 {
            format!("{pfx}{vname}()")
        } else {
            format!("{pfx}{vname}({})", self_bindings.join(", "))
        };

        let other_pattern = if field_count == 0 {
            format!("{pfx}{vname}()")
        } else {
            format!("{pfx}{vname}({})", other_bindings.join(", "))
        };

        // Build the comparison expression
        let comparison = if field_count == 0 {
            "true".to_string()
        } else {
            let comparisons: Vec<String> = self_bindings
                .iter()
                .zip(other_bindings.iter())
                .map(|(a, b)| format!("{a} == {b}"))
                .collect();
            comparisons.join(" and ")
        };

        arms.push_str(&format!(
            "            case {self_pattern}:\n\
             \x20               match other:\n\
             \x20                   case {other_pattern}:\n\
             \x20                       return {comparison}\n\
             \x20                   else:\n\
             \x20                       return false\n"
        ));
    }

    format!(
        "equip {gp}{type_name}{gs} with Equatable:\n\
         \x20   bool eq(self, {type_name}{gs} other):\n\
         \x20       match self:\n\
         {arms}"
    )
}

fn generate_enum_displayable(type_name: &str, gs: &str, e: &EnumDef) -> String {
    let gp = equip_generic_prefix(gs);
    let pfx = variant_prefix(type_name, e);
    let mut arms = String::new();

    for variant in &e.variants {
        let vname = &variant.node.name.node;
        let field_count = match &variant.node.fields {
            VariantFields::Unit => 0,
            VariantFields::Tuple(fields) => fields.len(),
        };

        let bindings: Vec<String> = (0..field_count).map(|i| format!("a{i}")).collect();

        let pattern = if field_count == 0 {
            format!("{pfx}{vname}()")
        } else {
            format!("{pfx}{vname}({})", bindings.join(", "))
        };

        let display_str = if field_count == 0 {
            format!("return \"{vname}()\"")
        } else {
            let parts: Vec<String> = bindings.iter().map(|b| format!("{{{b}}}")).collect();
            format!("return f\"{vname}({})\"", parts.join(", "))
        };

        arms.push_str(&format!(
            "            case {pattern}:\n\
             \x20               {display_str}\n"
        ));
    }

    format!(
        "equip {gp}{type_name}{gs} with Displayable:\n\
         \x20   String display(self):\n\
         \x20       match self:\n\
         {arms}"
    )
}

fn generate_enum_cloneable(type_name: &str, gs: &str, e: &EnumDef) -> String {
    let gp = equip_generic_prefix(gs);
    let pfx = variant_prefix(type_name, e);
    let mut arms = String::new();

    for variant in &e.variants {
        let vname = &variant.node.name.node;
        let field_count = match &variant.node.fields {
            VariantFields::Unit => 0,
            VariantFields::Tuple(fields) => fields.len(),
        };

        let bindings: Vec<String> = (0..field_count).map(|i| format!("a{i}")).collect();

        let pattern = if field_count == 0 {
            format!("{pfx}{vname}()")
        } else {
            format!("{pfx}{vname}({})", bindings.join(", "))
        };

        let reconstruction = if field_count == 0 {
            format!("return {pfx}{vname}()")
        } else {
            format!("return {pfx}{vname}({})", bindings.join(", "))
        };

        arms.push_str(&format!(
            "            case {pattern}:\n\
             \x20               {reconstruction}\n"
        ));
    }

    format!(
        "equip {gp}{type_name}{gs} with Cloneable:\n\
         \x20   {type_name}{gs} clone(self):\n\
         \x20       match self:\n\
         {arms}"
    )
}

fn generate_enum_hashable(type_name: &str, gs: &str, e: &EnumDef) -> String {
    let gp = equip_generic_prefix(gs);
    let pfx = variant_prefix(type_name, e);
    let mut arms = String::new();

    for (idx, variant) in e.variants.iter().enumerate() {
        let vname = &variant.node.name.node;
        let field_count = match &variant.node.fields {
            VariantFields::Unit => 0,
            VariantFields::Tuple(fields) => fields.len(),
        };

        let bindings: Vec<String> = (0..field_count).map(|i| format!("a{i}")).collect();

        let pattern = if field_count == 0 {
            format!("{pfx}{vname}()")
        } else {
            format!("{pfx}{vname}({})", bindings.join(", "))
        };

        let hash_body = if field_count == 0 {
            format!("return {idx}")
        } else {
            let mut lines = vec![format!("int h = {idx}")];
            for b in &bindings {
                lines.push(format!("                h = h *% 31 +% {b}.hash()"));
            }
            lines.push("                return h".to_string());
            lines.join("\n")
        };

        arms.push_str(&format!(
            "            case {pattern}:\n\
             \x20               {hash_body}\n"
        ));
    }

    format!(
        "equip {gp}{type_name}{gs} with Hashable:\n\
         \x20   int hash(self):\n\
         \x20       match self:\n\
         {arms}"
    )
}

fn generate_enum_serializable(type_name: &str, gs: &str, e: &EnumDef) -> String {
    let gp = equip_generic_prefix(gs);
    let pfx = variant_prefix(type_name, e);
    let mut arms = String::new();

    for variant in &e.variants {
        let vname = &variant.node.name.node;
        let field_count = match &variant.node.fields {
            VariantFields::Unit => 0,
            VariantFields::Tuple(fields) => fields.len(),
        };

        let bindings: Vec<String> = (0..field_count).map(|i| format!("a{i}")).collect();

        let pattern = if field_count == 0 {
            format!("{pfx}{vname}()")
        } else {
            format!("{pfx}{vname}({})", bindings.join(", "))
        };

        if field_count == 0 {
            // Unit variant → serialize as a string
            arms.push_str(&format!(
                "            case {pattern}:\n\
                 \x20               ser.write_str(\"{vname}\")\n"
            ));
        } else {
            // Data variant → externally-tagged: {"VariantName": [field0, field1, ...]}
            let field_types: Vec<&str> = match &variant.node.fields {
                VariantFields::Tuple(fields) => fields
                    .iter()
                    .map(|f| {
                        let ty = format_type(&f.node);
                        Box::leak(ty.into_boxed_str()) as &str
                    })
                    .collect(),
                VariantFields::Unit => vec![],
            };

            let mut body = format!(
                "                ser.begin_struct(\"{vname}\", 1)\n\
                 \x20               ser.field(\"{vname}\", 0)\n\
                 \x20               ser.begin_seq({field_count})\n"
            );
            for (i, binding) in bindings.iter().enumerate() {
                body.push_str(&format!("                ser.elem({i})\n"));
                body.push_str(&field_write_lines(
                    binding,
                    field_types[i],
                    "                ",
                ));
            }
            body.push_str(
                "                ser.end_seq()\n\
                 \x20               ser.end_struct()",
            );

            arms.push_str(&format!("            case {pattern}:\n{body}\n"));
        }
    }

    format!(
        "equip {gp}{type_name}{gs} with Serializable:\n\
         \x20   void serialize(self, Box[Serializer] ser):\n\
         \x20       match self:\n\
         {arms}"
    )
}

// ── Parsing helper ────────────────────────────────────────────

/// Parse a generated Gorget source string and extract EquipBlock and Function items.
///
/// Most derives generate equip blocks (trait implementations). `@derive(Deserializable)`
/// generates free functions instead (since Gorget has no `Self` type in traits).
fn parse_and_collect_derived_items(source: &str, new_items: &mut Vec<Spanned<Item>>) {
    // Each derive source gets a unique base offset so that internal spans
    // (method calls, expressions) don't collide across different derive-generated
    // items. Without this, method_resolutions keyed by span.start would clash
    // when multiple @derive attributes produce equip blocks with similar structure.
    let base_offset = DERIVE_SPAN_OFFSET.fetch_add(100_000, Ordering::Relaxed);
    let mut parser = Parser::new_with_offset(source, base_offset);
    let parsed = parser.parse_module();

    if !parser.errors.is_empty() {
        // This should never happen — it means the derive codegen is buggy.
        panic!(
            "derive: internal error: generated source failed to parse:\n{source}\nerrors: {:?}",
            parser.errors
        );
    }

    for item in parsed.items {
        if matches!(&item.node, Item::Equip(_) | Item::Function(_)) {
            new_items.push(item);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_struct_equatable_no_fields() {
        let src = generate_struct_equatable("Empty", "", &[]);
        assert!(src.contains("return true"));
        assert!(src.contains("equip Empty with Equatable"));
    }

    #[test]
    fn test_struct_equatable_two_fields() {
        let src = generate_struct_equatable("Point", "", &[("x", "float"), ("y", "float")]);
        assert!(src.contains("self.x == other.x and self.y == other.y"));
    }

    #[test]
    fn test_struct_displayable() {
        let src = generate_struct_displayable("Point", "", &[("x", "float"), ("y", "float")]);
        assert!(src.contains("f\"Point(x={self.x}, y={self.y})\""));
    }

    #[test]
    fn test_struct_cloneable() {
        let src = generate_struct_cloneable("Point", "", &[("x", "float"), ("y", "float")]);
        assert!(src.contains("return Point(self.x, self.y)"));
    }

    #[test]
    fn test_extract_derive_args() {
        let attrs = vec![Spanned::new(
            Attribute {
                name: Spanned::new("derive".to_string(), Span::dummy()),
                args: vec![
                    AttributeArg::Identifier("Equatable".to_string()),
                    AttributeArg::Identifier("Displayable".to_string()),
                ],
            },
            Span::dummy(),
        )];
        let result = extract_derive_args(&attrs);
        assert_eq!(result, vec!["Equatable", "Displayable"]);
    }

    #[test]
    fn test_generated_source_parses() {
        let src = generate_struct_equatable("Point", "", &[("x", "float"), ("y", "float")]);
        let mut parser = Parser::new(&src);
        let module = parser.parse_module();
        assert!(
            parser.errors.is_empty(),
            "parse errors: {:?}\nsource:\n{src}",
            parser.errors
        );
        assert!(module.items.iter().any(|i| matches!(&i.node, Item::Equip(_))));
    }

    #[test]
    fn test_enum_equatable_parses() {
        // Build a minimal EnumDef to test
        let source = "\
@derive(Equatable)
enum Color:
    Red()
    Green()
    Blue(int)

void main():
    pass
";
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        let mut errors = Vec::new();
        expand_derives(&mut module, &mut errors);
        assert!(errors.is_empty(), "derive errors: {:?}", errors);

        // Should have the original enum + main + the generated equip block
        let equip_count = module
            .items
            .iter()
            .filter(|i| matches!(&i.node, Item::Equip(_)))
            .count();
        assert_eq!(equip_count, 1, "expected 1 equip block");
    }

    #[test]
    fn test_enum_displayable_parses() {
        let source = "\
@derive(Displayable)
enum Color:
    Red()
    Green()
    Blue(int)

void main():
    pass
";
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        let mut errors = Vec::new();
        expand_derives(&mut module, &mut errors);
        assert!(errors.is_empty(), "derive errors: {:?}", errors);

        let equip_count = module
            .items
            .iter()
            .filter(|i| matches!(&i.node, Item::Equip(_)))
            .count();
        assert_eq!(equip_count, 1, "expected 1 equip block");
    }

    #[test]
    fn test_underivable_trait_error() {
        let source = "\
@derive(Drop)
struct Point:
    float x

void main():
    pass
";
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty());

        let mut errors = Vec::new();
        expand_derives(&mut module, &mut errors);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0].kind,
            SemanticErrorKind::UnderivableTrait { trait_name, type_name }
            if trait_name == "Drop" && type_name == "Point"
        ));
    }

    #[test]
    fn test_struct_hashable() {
        let src = generate_struct_hashable("Point", "", &[("x", "float"), ("y", "float")]);
        assert!(src.contains("equip Point with Hashable"));
        assert!(src.contains("int hash(self)"));
        assert!(src.contains("self.x.hash()"));
        assert!(src.contains("h *% 31 +% self.y.hash()"));
    }

    #[test]
    fn test_struct_hashable_single_field() {
        let src = generate_struct_hashable("Wrapper", "", &[("val", "int")]);
        assert!(src.contains("return self.val.hash()"));
    }

    #[test]
    fn test_struct_hashable_no_fields() {
        let src = generate_struct_hashable("Empty", "", &[]);
        assert!(src.contains("return 0"));
    }

    #[test]
    fn test_enum_cloneable_parses() {
        let source = "\
@derive(Cloneable)
enum Color:
    Red()
    Green()
    Blue(int)

void main():
    pass
";
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        let mut errors = Vec::new();
        expand_derives(&mut module, &mut errors);
        assert!(errors.is_empty(), "derive errors: {:?}", errors);

        let equip_count = module
            .items
            .iter()
            .filter(|i| matches!(&i.node, Item::Equip(_)))
            .count();
        assert_eq!(equip_count, 1, "expected 1 equip block");
    }

    #[test]
    fn test_enum_hashable_parses() {
        let source = "\
@derive(Hashable)
enum Color:
    Red()
    Green()
    Blue(int)

void main():
    pass
";
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        let mut errors = Vec::new();
        expand_derives(&mut module, &mut errors);
        assert!(errors.is_empty(), "derive errors: {:?}", errors);

        let equip_count = module
            .items
            .iter()
            .filter(|i| matches!(&i.node, Item::Equip(_)))
            .count();
        assert_eq!(equip_count, 1, "expected 1 equip block");
    }

    #[test]
    fn test_generic_struct_derive_parses() {
        let source = "\
@derive(Equatable, Displayable, Cloneable)
struct Pair[T]:
    T first
    T second

void main():
    pass
";
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        let mut errors = Vec::new();
        expand_derives(&mut module, &mut errors);
        assert!(errors.is_empty(), "derive errors: {:?}", errors);

        let equip_count = module
            .items
            .iter()
            .filter(|i| matches!(&i.node, Item::Equip(_)))
            .count();
        assert_eq!(equip_count, 3, "expected 3 equip blocks for 3 traits");
    }

    #[test]
    fn test_generic_enum_derive_parses() {
        let source = "\
@derive(Displayable, Cloneable)
enum Wrapper[T]:
    Empty()
    Value(T)

void main():
    pass
";
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        let mut errors = Vec::new();
        expand_derives(&mut module, &mut errors);
        assert!(errors.is_empty(), "derive errors: {:?}", errors);

        let equip_count = module
            .items
            .iter()
            .filter(|i| matches!(&i.node, Item::Equip(_)))
            .count();
        assert_eq!(equip_count, 2, "expected 2 equip blocks for 2 traits");
    }

    #[test]
    fn test_struct_equatable_generic() {
        let src = generate_struct_equatable("Pair", "[T]", &[("first", "T"), ("second", "T")]);
        assert!(src.contains("equip [T] Pair[T] with Equatable"));
        assert!(src.contains("Pair[T] other"));
    }

    #[test]
    fn test_struct_cloneable_generic() {
        let src = generate_struct_cloneable("Pair", "[T]", &[("first", "T"), ("second", "T")]);
        assert!(src.contains("equip [T] Pair[T] with Cloneable"));
        assert!(src.contains("Pair[T] clone(self)"));
    }

    #[test]
    fn test_struct_serializable() {
        let src = generate_struct_serializable("User", "", &[("name", "String"), ("age", "int")]);
        assert!(src.contains("equip User with Serializable"));
        assert!(src.contains("void serialize(self, Box[Serializer] ser)"));
        assert!(src.contains("ser.begin_struct(\"User\", 2)"));
        assert!(src.contains("ser.field(\"name\", 0)"));
        assert!(src.contains("ser.write_str(self.name)"));
        assert!(src.contains("ser.field(\"age\", 1)"));
        assert!(src.contains("ser.write_int(self.age)"));
        assert!(src.contains("ser.end_struct()"));
    }

    #[test]
    fn test_struct_serializable_parses() {
        let src = generate_struct_serializable(
            "Point",
            "",
            &[("x", "float"), ("y", "float"), ("label", "String")],
        );
        let mut parser = Parser::new(&src);
        let module = parser.parse_module();
        assert!(
            parser.errors.is_empty(),
            "parse errors: {:?}\nsource:\n{src}",
            parser.errors
        );
        assert!(module.items.iter().any(|i| matches!(&i.node, Item::Equip(_))));
    }

    #[test]
    fn test_field_write_call() {
        assert_eq!(field_write_call("self.x", "int"), "ser.write_int(self.x)");
        assert_eq!(field_write_call("self.x", "float"), "ser.write_float(self.x)");
        assert_eq!(field_write_call("self.x", "bool"), "ser.write_bool(self.x)");
        assert_eq!(field_write_call("self.x", "String"), "ser.write_str(self.x)");
        assert_eq!(field_write_call("self.x", "int8"), "ser.write_int(self.x as int)");
        assert_eq!(field_write_call("self.x", "uint64"), "ser.write_int(self.x as int)");
        assert_eq!(field_write_call("self.x", "float32"), "ser.write_float(self.x as float)");
        assert_eq!(field_write_call("self.x", "User"), "self.x.serialize(ser)");
    }

    #[test]
    fn test_enum_serializable_parses() {
        let source = "\
@derive(Serializable)
enum Color:
    Red()
    Green()
    Custom(int, int, int)

void main():
    pass
";
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        let mut errors = Vec::new();
        expand_derives(&mut module, &mut errors);
        assert!(errors.is_empty(), "derive errors: {:?}", errors);

        let equip_count = module
            .items
            .iter()
            .filter(|i| matches!(&i.node, Item::Equip(_)))
            .count();
        assert_eq!(equip_count, 1, "expected 1 equip block");
    }

    #[test]
    fn test_struct_default() {
        let src = generate_struct_default("Point", "", &[("x", "float"), ("y", "float")]);
        assert!(src.contains("equip Point with Default"));
        assert!(src.contains("Point default()"));
        assert!(src.contains("return Point(0.0, 0.0)"));
    }

    #[test]
    fn test_struct_default_parses() {
        let src =
            generate_struct_default("Config", "", &[("w", "int"), ("on", "bool"), ("n", "String")]);
        let mut parser = Parser::new(&src);
        let module = parser.parse_module();
        assert!(
            parser.errors.is_empty(),
            "parse errors: {:?}\nsource:\n{src}",
            parser.errors
        );
        assert!(module
            .items
            .iter()
            .any(|i| matches!(&i.node, Item::Equip(_))));
    }

    #[test]
    fn test_struct_default_nested() {
        let src =
            generate_struct_default("Wrapper", "", &[("inner", "Point"), ("count", "int")]);
        assert!(src.contains("Point.default()"));
        assert!(src.contains(", 0)"));
    }

    #[test]
    fn test_field_default_value() {
        assert_eq!(field_default_value("int"), "0");
        assert_eq!(field_default_value("uint8"), "0");
        assert_eq!(field_default_value("float"), "0.0");
        assert_eq!(field_default_value("float32"), "0.0");
        assert_eq!(field_default_value("bool"), "false");
        assert_eq!(field_default_value("String"), "String()");
        assert_eq!(field_default_value("Point"), "Point.default()");
    }

    #[test]
    fn test_struct_deserializable() {
        let src = generate_struct_deserializable("User", "", &[("name", "String"), ("age", "int")]);
        assert!(src.contains("Result[User, String] deserialize_User(Box[Deserializer] de)"));
        assert!(src.contains("de.begin_struct()"));
        assert!(src.contains("de.field(\"name\")"));
        assert!(src.contains("String name = de.read_str()"));
        assert!(src.contains("de.field(\"age\")"));
        assert!(src.contains("int age = de.read_int()"));
        assert!(src.contains("de.end_struct()"));
        assert!(src.contains("if de.has_error()"));
        assert!(src.contains("return Error(de.error())"));
        assert!(src.contains("return Ok(User(name, age))"));
    }

    #[test]
    fn test_struct_deserializable_parses() {
        let src = generate_struct_deserializable(
            "Point",
            "",
            &[("x", "float"), ("y", "float"), ("label", "String")],
        );
        let mut parser = Parser::new(&src);
        let module = parser.parse_module();
        assert!(
            parser.errors.is_empty(),
            "parse errors: {:?}\nsource:\n{src}",
            parser.errors
        );
        assert!(module.items.iter().any(|i| matches!(&i.node, Item::Function(_))));
    }

    #[test]
    fn test_struct_deserializable_nested() {
        let src = generate_struct_deserializable(
            "Profile",
            "",
            &[("label", "String"), ("user", "User")],
        );
        assert!(src.contains("User user = deserialize_User(de)"));
    }

    #[test]
    fn test_field_read_decl() {
        assert_eq!(field_read_decl("x", "int"), "int x = de.read_int()");
        assert_eq!(field_read_decl("x", "float"), "float x = de.read_float()");
        assert_eq!(field_read_decl("x", "bool"), "bool x = de.read_bool()");
        assert_eq!(field_read_decl("x", "String"), "String x = de.read_str()");
        assert_eq!(
            field_read_decl("x", "int8"),
            "int x_raw = de.read_int()\n    int8 x = x_raw as int8"
        );
        assert_eq!(
            field_read_decl("x", "float32"),
            "float x_raw = de.read_float()\n    float32 x = x_raw as float32"
        );
        assert_eq!(field_read_decl("x", "User"), "User x = deserialize_User(de)");
    }

    #[test]
    fn test_enum_deserializable_parses() {
        let source = "\
@derive(Deserializable)
enum Color:
    Red()
    Green()
    Custom(int, int, int)

void main():
    pass
";
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);

        let mut errors = Vec::new();
        expand_derives(&mut module, &mut errors);
        assert!(errors.is_empty(), "derive errors: {:?}", errors);

        let fn_count = module
            .items
            .iter()
            .filter(|i| matches!(&i.node, Item::Function(_)))
            .count();
        // main() + deserialize_Color
        assert_eq!(fn_count, 2, "expected 2 functions (main + deserialize_Color)");
    }

    #[test]
    fn test_struct_from() {
        let src = generate_struct_from("Celsius", "", &[("value", "float")]);
        assert!(src.contains("equip Celsius with From[float]"));
        assert!(src.contains("Celsius from(float value)"));
        assert!(src.contains("return Celsius(value)"));
    }

    #[test]
    fn test_struct_from_parses() {
        let src = generate_struct_from("UserId", "", &[("id", "int")]);
        let mut parser = Parser::new(&src);
        let module = parser.parse_module();
        assert!(
            parser.errors.is_empty(),
            "parse errors: {:?}\nsource:\n{src}",
            parser.errors
        );
        assert!(module.items.iter().any(|i| matches!(&i.node, Item::Equip(_))));
    }

    #[test]
    fn test_struct_try_from() {
        let src = generate_struct_try_from("Celsius", "", &[("value", "float")]);
        assert!(src.contains("equip Celsius with TryFrom[float]"));
        assert!(src.contains("Result[Celsius, String] try_from(float value)"));
        assert!(src.contains("return Ok(Celsius(value))"));
    }

    #[test]
    fn test_struct_try_from_parses() {
        let src = generate_struct_try_from("UserId", "", &[("id", "int")]);
        let mut parser = Parser::new(&src);
        let module = parser.parse_module();
        assert!(
            parser.errors.is_empty(),
            "parse errors: {:?}\nsource:\n{src}",
            parser.errors
        );
        assert!(module.items.iter().any(|i| matches!(&i.node, Item::Equip(_))));
    }

    #[test]
    fn test_derive_from_multi_field_error() {
        let source = "\
@derive(From)
struct Pair:
    int x
    int y

void main():
    pass
";
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty());

        let mut errors = Vec::new();
        expand_derives(&mut module, &mut errors);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0].kind,
            SemanticErrorKind::DeriveFromRequiresSingleField { type_name }
            if type_name == "Pair"
        ));
    }
}
