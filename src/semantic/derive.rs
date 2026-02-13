use crate::parser::ast::{
    Attribute, AttributeArg, EnumDef, Item, Module, StructDef, VariantFields,
};
use crate::parser::Parser;
use crate::span::{Span, Spanned};

use super::errors::{SemanticError, SemanticErrorKind};

/// Expand all `@derive(...)` attributes on structs and enums in the module.
///
/// For each derivable trait, generates Gorget source code for the equip block,
/// parses it, and appends the resulting `EquipBlock` items to the module.
pub fn expand_derives(module: &mut Module, errors: &mut Vec<SemanticError>) {
    let mut new_items = Vec::new();

    for item in &module.items {
        match &item.node {
            Item::Struct(s) => {
                collect_struct_derives(s, &mut new_items, errors);
            }
            Item::Enum(e) => {
                collect_enum_derives(e, &mut new_items, errors);
            }
            _ => {}
        }
    }

    module.items.extend(new_items);
}

const DERIVABLE_STRUCT_TRAITS: &[&str] = &["Equatable", "Displayable", "Cloneable"];
const DERIVABLE_ENUM_TRAITS: &[&str] = &["Equatable", "Displayable"];

fn collect_struct_derives(
    s: &StructDef,
    new_items: &mut Vec<Spanned<Item>>,
    errors: &mut Vec<SemanticError>,
) {
    let type_name = &s.name.node;

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

        let source = generate_struct_derive(type_name, &trait_name, &fields);
        parse_and_collect_equip_blocks(&source, new_items);
    }
}

fn collect_enum_derives(
    e: &EnumDef,
    new_items: &mut Vec<Spanned<Item>>,
    errors: &mut Vec<SemanticError>,
) {
    let type_name = &e.name.node;

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

        let source = generate_enum_derive(type_name, &trait_name, e);
        parse_and_collect_equip_blocks(&source, new_items);
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
            PrimitiveType::Char => "char".to_string(),
            PrimitiveType::Str => "str".to_string(),
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

// ── Struct derive generation ──────────────────────────────────

fn generate_struct_derive(type_name: &str, trait_name: &str, fields: &[(&str, &str)]) -> String {
    match trait_name {
        "Equatable" => generate_struct_equatable(type_name, fields),
        "Displayable" => generate_struct_displayable(type_name, fields),
        "Cloneable" => generate_struct_cloneable(type_name, fields),
        _ => String::new(),
    }
}

fn generate_struct_equatable(type_name: &str, fields: &[(&str, &str)]) -> String {
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
        "equip {type_name} with Equatable:\n    bool eq(self, {type_name} other):\n{body}\n"
    )
}

fn generate_struct_displayable(type_name: &str, fields: &[(&str, &str)]) -> String {
    let body = if fields.is_empty() {
        format!("        return \"{type_name}()\"")
    } else {
        let parts: Vec<String> = fields
            .iter()
            .map(|(name, _)| format!("{name}={{self.{name}}}"))
            .collect();
        format!(
            "        return format(\"{type_name}({})\")",
            parts.join(", ")
        )
    };

    format!(
        "equip {type_name} with Displayable:\n    str display(self):\n{body}\n"
    )
}

fn generate_struct_cloneable(type_name: &str, fields: &[(&str, &str)]) -> String {
    let args: Vec<String> = fields.iter().map(|(name, _)| format!("self.{name}")).collect();
    let body = format!("        return {type_name}({})", args.join(", "));

    format!(
        "equip {type_name} with Cloneable:\n    {type_name} clone(self):\n{body}\n"
    )
}

// ── Enum derive generation ────────────────────────────────────

fn generate_enum_derive(type_name: &str, trait_name: &str, e: &EnumDef) -> String {
    match trait_name {
        "Equatable" => generate_enum_equatable(type_name, e),
        "Displayable" => generate_enum_displayable(type_name, e),
        _ => String::new(),
    }
}

fn generate_enum_equatable(type_name: &str, e: &EnumDef) -> String {
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
            format!("{vname}()")
        } else {
            format!("{vname}({})", self_bindings.join(", "))
        };

        let other_pattern = if field_count == 0 {
            format!("{vname}()")
        } else {
            format!("{vname}({})", other_bindings.join(", "))
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
        "equip {type_name} with Equatable:\n\
         \x20   bool eq(self, {type_name} other):\n\
         \x20       match self:\n\
         {arms}"
    )
}

fn generate_enum_displayable(type_name: &str, e: &EnumDef) -> String {
    let mut arms = String::new();

    for variant in &e.variants {
        let vname = &variant.node.name.node;
        let field_count = match &variant.node.fields {
            VariantFields::Unit => 0,
            VariantFields::Tuple(fields) => fields.len(),
        };

        let bindings: Vec<String> = (0..field_count).map(|i| format!("a{i}")).collect();

        let pattern = if field_count == 0 {
            format!("{vname}()")
        } else {
            format!("{vname}({})", bindings.join(", "))
        };

        let display_str = if field_count == 0 {
            format!("return \"{vname}()\"")
        } else {
            let parts: Vec<String> = bindings.iter().map(|b| format!("{{{b}}}")).collect();
            format!("return format(\"{vname}({})\")", parts.join(", "))
        };

        arms.push_str(&format!(
            "            case {pattern}:\n\
             \x20               {display_str}\n"
        ));
    }

    format!(
        "equip {type_name} with Displayable:\n\
         \x20   str display(self):\n\
         \x20       match self:\n\
         {arms}"
    )
}

// ── Parsing helper ────────────────────────────────────────────

/// Parse a generated Gorget source string and extract all EquipBlock items.
fn parse_and_collect_equip_blocks(source: &str, new_items: &mut Vec<Spanned<Item>>) {
    let mut parser = Parser::new(source);
    let parsed = parser.parse_module();

    if !parser.errors.is_empty() {
        // This should never happen — it means the derive codegen is buggy.
        panic!(
            "derive: internal error: generated source failed to parse:\n{source}\nerrors: {:?}",
            parser.errors
        );
    }

    for item in parsed.items {
        if matches!(&item.node, Item::Equip(_)) {
            // Re-wrap with a dummy span so it doesn't point into the original source
            new_items.push(Spanned::new(item.node, Span::dummy()));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_struct_equatable_no_fields() {
        let src = generate_struct_equatable("Empty", &[]);
        assert!(src.contains("return true"));
        assert!(src.contains("equip Empty with Equatable"));
    }

    #[test]
    fn test_struct_equatable_two_fields() {
        let src = generate_struct_equatable("Point", &[("x", "float"), ("y", "float")]);
        assert!(src.contains("self.x == other.x and self.y == other.y"));
    }

    #[test]
    fn test_struct_displayable() {
        let src = generate_struct_displayable("Point", &[("x", "float"), ("y", "float")]);
        assert!(src.contains("Point(x={self.x}, y={self.y})"));
    }

    #[test]
    fn test_struct_cloneable() {
        let src = generate_struct_cloneable("Point", &[("x", "float"), ("y", "float")]);
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
        let src = generate_struct_equatable("Point", &[("x", "float"), ("y", "float")]);
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
@derive(Hashable)
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
            if trait_name == "Hashable" && type_name == "Point"
        ));
    }
}
