/// Name mangling for C identifiers.

/// C11 reserved keywords that must be escaped.
const C_KEYWORDS: &[&str] = &[
    "auto", "break", "case", "char", "const", "continue", "default", "do", "double", "else",
    "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long", "register",
    "restrict", "return", "short", "signed", "sizeof", "static", "struct", "switch", "typedef",
    "union", "unsigned", "void", "volatile", "while", "_Bool", "_Complex", "_Imaginary",
    "_Alignas", "_Alignof", "_Atomic", "_Generic", "_Noreturn", "_Static_assert",
    "_Thread_local",
];

/// Escape a name if it collides with a C keyword.
pub fn escape_keyword(name: &str) -> String {
    if C_KEYWORDS.contains(&name) {
        format!("__vyper_{name}")
    } else {
        name.to_string()
    }
}

/// Mangle a method name: `Point__distance`
pub fn mangle_method(type_name: &str, method_name: &str) -> String {
    format!("{type_name}__{method_name}")
}

/// Mangle a trait impl method: `Displayable_for_Point__to_string`
pub fn mangle_trait_method(trait_name: &str, type_name: &str, method_name: &str) -> String {
    format!("{trait_name}_for_{type_name}__{method_name}")
}

/// Mangle an enum variant: `Color__Red`
pub fn mangle_variant(enum_name: &str, variant_name: &str) -> String {
    format!("{enum_name}__{variant_name}")
}

/// Mangle an enum tag: `Color_TAG_Red`
pub fn mangle_tag(enum_name: &str, variant_name: &str) -> String {
    format!("{enum_name}_TAG_{variant_name}")
}

/// Mangle enum data struct: `Color_Red_Data`
pub fn mangle_variant_data(enum_name: &str, variant_name: &str) -> String {
    format!("{enum_name}_{variant_name}_Data")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keyword_escaping() {
        assert_eq!(escape_keyword("int"), "__vyper_int");
        assert_eq!(escape_keyword("return"), "__vyper_return");
        assert_eq!(escape_keyword("myvar"), "myvar");
    }

    #[test]
    fn method_mangling() {
        assert_eq!(mangle_method("Point", "distance"), "Point__distance");
        assert_eq!(
            mangle_trait_method("Displayable", "Point", "to_string"),
            "Displayable_for_Point__to_string"
        );
    }

    #[test]
    fn variant_mangling() {
        assert_eq!(mangle_variant("Color", "Red"), "Color__Red");
        assert_eq!(mangle_tag("Color", "Red"), "Color_TAG_Red");
        assert_eq!(mangle_variant_data("Color", "Red"), "Color_Red_Data");
    }
}
