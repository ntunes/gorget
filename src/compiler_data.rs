//! Canonical resource + runtime-function table embedded into the binary.
//!
//! The data lives in `compiler/data/{schema,resources}.gg` as idiomatic
//! Gorget source. Bake it in here via `include_str!` so the shipped `gg`
//! binary is self-contained. A future `src/ir/resources.rs` loader will
//! parse this source with the compiler's own parser, walk the AST, and
//! produce the typed `ResourceTable`.
//!
//! Today this module only carries the embedded source strings and a
//! parse-only sanity test. Wiring the parser + AST walker is item 4 of
//! the resources.toml plan (TODO.md).

pub const SCHEMA_SRC: &str = include_str!("../compiler/data/schema.gg");
pub const RESOURCES_SRC: &str = include_str!("../compiler/data/resources.gg");

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::parser::ast::Item;

    /// `compiler/data/schema.gg` is valid Gorget source.
    #[test]
    fn schema_source_parses() {
        let mut parser = Parser::new(SCHEMA_SRC);
        let module = parser.parse_module();
        assert!(
            parser.errors.is_empty(),
            "schema.gg parse errors: {:?}",
            parser.errors,
        );

        // Spot-check: the spike-required types must be declared.
        let mut struct_names = Vec::new();
        let mut enum_names = Vec::new();
        for item in &module.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Enum(e) => enum_names.push(e.name.node.clone()),
                _ => {}
            }
        }

        for expected in &["ResourceMetadata", "ResourceEntry", "RuntimeFn", "RuntimeParam"] {
            assert!(
                struct_names.iter().any(|n| n == expected),
                "schema.gg missing struct {}: have {:?}",
                expected, struct_names,
            );
        }
        for expected in &[
            "MatchKind", "CopySemantics", "CollectionKind", "BoxKind", "LirType",
            "CRuntimeType", "AbiKind", "SideEffects",
        ] {
            assert!(
                enum_names.iter().any(|n| n == expected),
                "schema.gg missing enum {}: have {:?}",
                expected, enum_names,
            );
        }
    }

    /// `compiler/data/resources.gg` is valid Gorget source. (Full semantic
    /// check via `gg check` is deferred until the loader recognizes
    /// `compiler.data.*` imports — that's item 4 of the plan.)
    #[test]
    fn resources_source_parses() {
        let mut parser = Parser::new(RESOURCES_SRC);
        let module = parser.parse_module();
        assert!(
            parser.errors.is_empty(),
            "resources.gg parse errors: {:?}",
            parser.errors,
        );

        // Spot-check: the three required public-static declarations exist.
        let mut static_names = Vec::new();
        for item in &module.items {
            if let Item::StaticDecl(s) = &item.node {
                static_names.push(s.name.node.clone());
            }
        }
        for expected in &["SCHEMA_VERSION", "RESOURCES", "RUNTIME_FNS"] {
            assert!(
                static_names.iter().any(|n| n == expected),
                "resources.gg missing static {}: have {:?}",
                expected, static_names,
            );
        }
    }
}
