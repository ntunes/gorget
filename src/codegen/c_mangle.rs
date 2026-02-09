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
        format!("__gorget_{name}")
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

/// Mangle a closure function name: `__gorget_closure_0`
pub fn mangle_closure(id: usize) -> String {
    format!("__gorget_closure_{id}")
}

/// Mangle a closure environment struct name: `__gorget_env_0`
pub fn mangle_closure_env(id: usize) -> String {
    format!("__gorget_env_{id}")
}

/// Mangle a vtable struct name: `Shape_VTable`
pub fn mangle_vtable_struct(trait_name: &str) -> String {
    format!("{trait_name}_VTable")
}

/// Mangle a trait object struct name: `Shape_TraitObj`
pub fn mangle_trait_obj(trait_name: &str) -> String {
    format!("{trait_name}_TraitObj")
}

/// Mangle a vtable instance name: `Shape_for_Circle_vtable`
pub fn mangle_vtable_instance(trait_name: &str, type_name: &str) -> String {
    format!("{trait_name}_for_{type_name}_vtable")
}

/// Mangle a generic type/function instantiation.
/// `Pair` + `["int64_t", "double"]` → `Pair__int64_t__double`
pub fn mangle_generic(base: &str, c_type_args: &[String]) -> String {
    if c_type_args.is_empty() {
        return base.to_string();
    }
    let mangled: Vec<String> = c_type_args.iter().map(|t| sanitize_c_type(t)).collect();
    format!("{base}__{}", mangled.join("__"))
}

/// Sanitize a C type string for embedding in an identifier.
fn sanitize_c_type(t: &str) -> String {
    t.replace("const ", "const_")
        .replace("char*", "char_ptr")
        .replace('*', "_ptr")
        .replace(' ', "_")
        .replace('[', "_")
        .replace(']', "")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keyword_escaping() {
        assert_eq!(escape_keyword("int"), "__gorget_int");
        assert_eq!(escape_keyword("return"), "__gorget_return");
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

    #[test]
    fn closure_mangling() {
        assert_eq!(mangle_closure(0), "__gorget_closure_0");
        assert_eq!(mangle_closure(5), "__gorget_closure_5");
        assert_eq!(mangle_closure_env(0), "__gorget_env_0");
        assert_eq!(mangle_closure_env(3), "__gorget_env_3");
    }

    #[test]
    fn vtable_mangling() {
        assert_eq!(mangle_vtable_struct("Shape"), "Shape_VTable");
        assert_eq!(mangle_trait_obj("Shape"), "Shape_TraitObj");
        assert_eq!(
            mangle_vtable_instance("Shape", "Circle"),
            "Shape_for_Circle_vtable"
        );
        assert_eq!(mangle_vtable_struct("Drawable"), "Drawable_VTable");
        assert_eq!(mangle_trait_obj("Drawable"), "Drawable_TraitObj");
        assert_eq!(
            mangle_vtable_instance("Drawable", "Square"),
            "Drawable_for_Square_vtable"
        );
    }

    #[test]
    fn generic_mangle_basic() {
        assert_eq!(
            mangle_generic("Pair", &["int64_t".into(), "double".into()]),
            "Pair__int64_t__double"
        );
        assert_eq!(mangle_generic("Box", &["int64_t".into()]), "Box__int64_t");
        // Empty args returns base name unchanged
        assert_eq!(mangle_generic("Pair", &[]), "Pair");
    }

    #[test]
    fn generic_mangle_pointer_type() {
        assert_eq!(
            mangle_generic("Box", &["const char*".into()]),
            "Box__const_char_ptr"
        );
        assert_eq!(
            mangle_generic("Wrapper", &["int64_t*".into()]),
            "Wrapper__int64_t_ptr"
        );
    }
}
