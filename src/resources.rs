//! Load `compiler/data/resources.gg` at compiler-runtime and expose
//! a typed `ResourceTable` via a `OnceLock`-cached accessor.
//!
//! The source file lives at `compiler/data/resources.gg` and is baked
//! into the binary via `include_str!` (see `crate::compiler_data`).
//! At first call to `table()` we parse it with the compiler's own
//! parser, walk the AST, and convert the literal-only subset into
//! typed Rust structs from [`crate::resource_schema`].
//!
//! Development override: setting `GORGET_RESOURCES_PATH=<path>` reads
//! the file from disk instead of using the embedded copy, letting you
//! edit `resources.gg` and re-run without recompiling Rust.
//!
//! Schema discipline: this loader expects `SCHEMA_VERSION` in
//! `resources.gg` to match [`SCHEMA_VERSION_EXPECTED`]. Bumping the
//! schema means a coordinated edit across `compiler/data/schema.gg`,
//! `compiler/data/resources.gg`, [`crate::resource_schema`], and
//! this file's `SCHEMA_VERSION_EXPECTED`.

use std::sync::OnceLock;

use crate::compiler_data::RESOURCES_SRC;
use crate::resource_schema::*;
use crate::parser::ast::{CallArg, Expr, Item};
use crate::parser::Parser;
use crate::span::Spanned;

pub const SCHEMA_VERSION_EXPECTED: u32 = 2;

static TABLE: OnceLock<ResourceTable> = OnceLock::new();

/// Lazily-loaded canonical resource table. First call parses the
/// embedded `resources.gg` source; subsequent calls are O(1) lookups.
///
/// Panics if the source fails to parse, the schema version mismatches,
/// or any entry has the wrong shape. These are build-system-level
/// errors — they indicate a coordinated update of the schema slipped.
pub fn table() -> &'static ResourceTable {
    TABLE.get_or_init(load_table)
}

fn load_table() -> ResourceTable {
    let owned: String;
    let src: &str = match std::env::var("GORGET_RESOURCES_PATH") {
        Ok(path) => {
            owned = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!(
                    "GORGET_RESOURCES_PATH={}: {}", path, e,
                ));
            &owned
        }
        Err(_) => RESOURCES_SRC,
    };

    let mut parser = Parser::new(src);
    let module = parser.parse_module();
    if !parser.errors.is_empty() {
        panic!("compiler/data/resources.gg parse errors: {:?}", parser.errors);
    }

    let table = walk_module(&module.items).unwrap_or_else(|e| {
        panic!("compiler/data/resources.gg schema error: {}", e);
    });

    if table.schema_version != SCHEMA_VERSION_EXPECTED {
        panic!(
            "resources.gg SCHEMA_VERSION {} != Rust SCHEMA_VERSION_EXPECTED {}",
            table.schema_version, SCHEMA_VERSION_EXPECTED,
        );
    }

    table
}

// ── AST walker ──────────────────────────────────────────────────────
//
// The data file uses a deliberately-restricted Gorget subset (literals
// + struct/enum constructor calls + vector literals). The walker
// matches these `Expr` shapes positionally — every constructor's
// argument list is mapped to its Rust struct's fields in declaration
// order. Field-count or unknown-variant errors are reported with the
// constructor name so a mistyped entry in `resources.gg` is easy to
// locate.

type WalkResult<T> = Result<T, String>;

fn walk_module(items: &[Spanned<Item>]) -> WalkResult<ResourceTable> {
    let mut schema_version: Option<u32> = None;
    let mut resources: Option<Vec<ResourceEntry>> = None;
    let mut runtime_fns: Option<Vec<RuntimeFn>> = None;

    for item in items {
        let Item::StaticDecl(s) = &item.node else { continue };
        match s.name.node.as_str() {
            "SCHEMA_VERSION" => {
                schema_version = Some(u32::try_from(as_int(&s.value.node)?)
                    .map_err(|_| "SCHEMA_VERSION must fit in u32".to_string())?);
            }
            "RESOURCES" => {
                let elems = as_array(&s.value.node)?;
                resources = Some(
                    elems.iter()
                        .map(|e| build_resource_entry(&e.node))
                        .collect::<WalkResult<Vec<_>>>()?
                );
            }
            "RUNTIME_FNS" => {
                let elems = as_array(&s.value.node)?;
                runtime_fns = Some(
                    elems.iter()
                        .map(|e| build_runtime_fn(&e.node))
                        .collect::<WalkResult<Vec<_>>>()?
                );
            }
            _ => {} // unknown static — ignore (lets the file grow)
        }
    }

    Ok(ResourceTable {
        schema_version: schema_version
            .ok_or("missing SCHEMA_VERSION static")?,
        resources: resources
            .ok_or("missing RESOURCES static")?,
        runtime_fns: runtime_fns
            .ok_or("missing RUNTIME_FNS static")?,
    })
}

// ── Generic extractors ──────────────────────────────────────────────

fn as_int(e: &Expr) -> WalkResult<i64> {
    match e {
        Expr::IntLiteral(n) => Ok(*n),
        other => Err(format!("expected int literal, got {}", expr_kind(other))),
    }
}

fn as_bool(e: &Expr) -> WalkResult<bool> {
    match e {
        Expr::BoolLiteral(b) => Ok(*b),
        other => Err(format!("expected bool literal, got {}", expr_kind(other))),
    }
}

fn as_string(e: &Expr) -> WalkResult<String> {
    match e {
        Expr::StringLiteral(lit, _) => Ok(lit.as_plain_text()),
        other => Err(format!("expected string literal, got {}", expr_kind(other))),
    }
}

fn as_option_string(e: &Expr) -> WalkResult<Option<String>> {
    match e {
        Expr::NoneLiteral => Ok(None),
        _ => {
            let (name, args) = as_call(e)?;
            if name != "Some" {
                return Err(format!("expected Some(...) or None, got {}", name));
            }
            expect_arity(name, args, 1)?;
            Ok(Some(as_string(arg(args, 0)?)?))
        }
    }
}

fn as_array<'a>(e: &'a Expr) -> WalkResult<&'a [Spanned<Expr>]> {
    match e {
        Expr::ArrayLiteral(v, _) => Ok(v.as_slice()),
        other => Err(format!("expected [...] literal, got {}", expr_kind(other))),
    }
}

/// Extract `(name, args)` from a `Call { callee: Identifier(name), args }`
/// expression. The args slice is the underlying `Vec<Spanned<CallArg>>`;
/// helpers like [`arg`] unwrap to the inner `Expr`.
fn as_call(e: &Expr) -> WalkResult<(&str, &[Spanned<CallArg>])> {
    let Expr::Call { callee, args, .. } = e else {
        return Err(format!("expected constructor call, got {}", expr_kind(e)));
    };
    let Expr::Identifier(name) = &callee.node else {
        return Err(format!("expected bare-identifier callee, got {}", expr_kind(&callee.node)));
    };
    Ok((name.as_str(), args.as_slice()))
}

fn expect_arity(name: &str, args: &[Spanned<CallArg>], expected: usize) -> WalkResult<()> {
    if args.len() != expected {
        return Err(format!(
            "{}: expected {} arg(s), got {}", name, expected, args.len(),
        ));
    }
    Ok(())
}

/// Get the i-th argument's `Expr`. Named arguments and ownership sigils
/// aren't expected in the data file; we read positionally.
fn arg<'a>(args: &'a [Spanned<CallArg>], i: usize) -> WalkResult<&'a Expr> {
    let a = args.get(i).ok_or_else(|| format!("missing arg {}", i))?;
    if a.node.name.is_some() {
        return Err(format!("arg {}: named arguments not allowed in resources.gg", i));
    }
    Ok(&a.node.value.node)
}

fn expr_kind(e: &Expr) -> &'static str {
    match e {
        Expr::IntLiteral(_) => "int literal",
        Expr::FloatLiteral(_) => "float literal",
        Expr::BoolLiteral(_) => "bool literal",
        Expr::StringLiteral(..) => "string literal",
        Expr::NoneLiteral => "None",
        Expr::Identifier(_) => "identifier",
        Expr::Call { .. } => "call",
        Expr::ArrayLiteral(_, _) => "array literal",
        Expr::MethodCall { .. } => "method call",
        Expr::FieldAccess { .. } => "field access",
        Expr::BinaryOp { .. } => "binary op",
        Expr::UnaryOp { .. } => "unary op",
        _ => "<unsupported expr>",
    }
}

// ── Enum builders ───────────────────────────────────────────────────

fn build_match_kind(e: &Expr) -> WalkResult<MatchKind> {
    let (name, args) = as_call(e)?;
    match name {
        "MkExact" => {
            expect_arity(name, args, 1)?;
            Ok(MatchKind::Exact(as_string(arg(args, 0)?)?))
        }
        "MkPrefix" => {
            expect_arity(name, args, 1)?;
            Ok(MatchKind::Prefix(as_string(arg(args, 0)?)?))
        }
        other => Err(format!("unknown MatchKind variant: {}", other)),
    }
}

fn build_copy_semantics(e: &Expr) -> WalkResult<CopySemantics> {
    let (name, args) = as_call(e)?;
    expect_arity(name, args, 0)?;
    match name {
        "CsTrivial" => Ok(CopySemantics::Trivial),
        "CsResource" => Ok(CopySemantics::Resource),
        "CsRefCounted" => Ok(CopySemantics::RefCounted),
        other => Err(format!("unknown CopySemantics variant: {}", other)),
    }
}

fn build_collection_kind(e: &Expr) -> WalkResult<CollectionKind> {
    let (name, args) = as_call(e)?;
    expect_arity(name, args, 0)?;
    match name {
        "CkNotCollection" => Ok(CollectionKind::NotCollection),
        "CkVector" => Ok(CollectionKind::Vector),
        "CkDeque" => Ok(CollectionKind::Deque),
        "CkHeap" => Ok(CollectionKind::Heap),
        "CkDict" => Ok(CollectionKind::Dict),
        "CkOrderedSet" => Ok(CollectionKind::OrderedSet),
        "CkHashSet" => Ok(CollectionKind::HashSet),
        other => Err(format!("unknown CollectionKind variant: {}", other)),
    }
}

fn build_box_kind(e: &Expr) -> WalkResult<BoxKind> {
    let (name, args) = as_call(e)?;
    expect_arity(name, args, 0)?;
    match name {
        "BkNotBox" => Ok(BoxKind::NotBox),
        "BkRegularBox" => Ok(BoxKind::RegularBox),
        "BkTraitBox" => Ok(BoxKind::TraitBox),
        other => Err(format!("unknown BoxKind variant: {}", other)),
    }
}

fn build_lir_type(e: &Expr) -> WalkResult<LirType> {
    let (name, args) = as_call(e)?;
    expect_arity(name, args, 0)?;
    match name {
        "LtStructBase" => Ok(LirType::StructBase),
        "LtPtr" => Ok(LirType::Ptr),
        other => Err(format!("unknown LirType variant: {}", other)),
    }
}

fn build_c_runtime_type(e: &Expr) -> WalkResult<CRuntimeType> {
    let (name, args) = as_call(e)?;
    expect_arity(name, args, 0)?;
    Ok(match name {
        "TVoid" => CRuntimeType::Void,
        "TBool" => CRuntimeType::Bool,
        "TI8" => CRuntimeType::I8,
        "TI16" => CRuntimeType::I16,
        "TI32" => CRuntimeType::I32,
        "TI64" => CRuntimeType::I64,
        "TU8" => CRuntimeType::U8,
        "TU16" => CRuntimeType::U16,
        "TU32" => CRuntimeType::U32,
        "TU64" => CRuntimeType::U64,
        "TF32" => CRuntimeType::F32,
        "TF64" => CRuntimeType::F64,
        "TSize" => CRuntimeType::Size,
        "TPtr" => CRuntimeType::Ptr,
        "TVoidElem" => CRuntimeType::VoidElem,
        "TCStr" => CRuntimeType::CStr,
        "TStr" => CRuntimeType::Str,
        "TArray" => CRuntimeType::Array,
        "TMap" => CRuntimeType::Map,
        "TSet" => CRuntimeType::Set,
        "TRegex" => CRuntimeType::Regex,
        "TMatch" => CRuntimeType::Match,
        other => return Err(format!("unknown CRuntimeType variant: {}", other)),
    })
}

fn build_abi_kind(e: &Expr) -> WalkResult<AbiKind> {
    let (name, args) = as_call(e)?;
    expect_arity(name, args, 0)?;
    Ok(match name {
        "AAuto" => AbiKind::Auto,
        "AScalar" => AbiKind::Scalar,
        "APtr" => AbiKind::Ptr,
        "AOpaque" => AbiKind::Opaque,
        "AGorgetString" => AbiKind::GorgetString,
        "AVoidElem" => AbiKind::VoidElem,
        "ACStr" => AbiKind::CStr,
        "AByValue" => AbiKind::ByValue,
        other => return Err(format!("unknown AbiKind variant: {}", other)),
    })
}

fn build_side_effects(e: &Expr) -> WalkResult<SideEffects> {
    let (name, args) = as_call(e)?;
    expect_arity(name, args, 0)?;
    Ok(match name {
        "FxPure" => SideEffects::Pure,
        "FxReadOnly" => SideEffects::ReadOnly,
        "FxAllocates" => SideEffects::Allocates,
        "FxMutates" => SideEffects::Mutates,
        "FxIo" => SideEffects::Io,
        "FxAborts" => SideEffects::Aborts,
        "FxConcurrent" => SideEffects::Concurrent,
        "FxUnknown" => SideEffects::Unknown,
        other => return Err(format!("unknown SideEffects variant: {}", other)),
    })
}

// ── Struct builders ─────────────────────────────────────────────────

fn build_resource_metadata(e: &Expr) -> WalkResult<ResourceMetadata> {
    let (name, args) = as_call(e)?;
    if name != "ResourceMetadata" {
        return Err(format!("expected ResourceMetadata(...), got {}(...)", name));
    }
    expect_arity(name, args, 13)?;
    Ok(ResourceMetadata {
        runtime_name: as_string(arg(args, 0)?)?,
        size_bytes: u32::try_from(as_int(arg(args, 1)?)?)
            .map_err(|_| "size_bytes must fit in u32".to_string())?,
        lir_type: build_lir_type(arg(args, 2)?)?,
        drop_fn: as_option_string(arg(args, 3)?)?,
        clone_fn: as_option_string(arg(args, 4)?)?,
        materialize_fn: as_option_string(arg(args, 5)?)?,
        copy_semantics: build_copy_semantics(arg(args, 6)?)?,
        collection_kind: build_collection_kind(arg(args, 7)?)?,
        box_kind: build_box_kind(arg(args, 8)?)?,
        opaque_handle: as_bool(arg(args, 9)?)?,
        method_prefix: as_option_string(arg(args, 10)?)?,
        c_typedef_name: as_option_string(arg(args, 11)?)?,
        is_typed_constructor: as_bool(arg(args, 12)?)?,
    })
}

fn build_resource_entry(e: &Expr) -> WalkResult<ResourceEntry> {
    let (name, args) = as_call(e)?;
    if name != "ResourceEntry" {
        return Err(format!("expected ResourceEntry(...), got {}(...)", name));
    }
    expect_arity(name, args, 2)?;
    let match_on = as_array(arg(args, 0)?)?
        .iter()
        .map(|m| build_match_kind(&m.node))
        .collect::<WalkResult<Vec<_>>>()?;
    let metadata = build_resource_metadata(arg(args, 1)?)?;
    Ok(ResourceEntry { match_on, metadata })
}

fn build_runtime_param(e: &Expr) -> WalkResult<RuntimeParam> {
    let (name, args) = as_call(e)?;
    if name != "RuntimeParam" {
        return Err(format!("expected RuntimeParam(...), got {}(...)", name));
    }
    expect_arity(name, args, 2)?;
    Ok(RuntimeParam {
        ty: build_c_runtime_type(arg(args, 0)?)?,
        abi: build_abi_kind(arg(args, 1)?)?,
    })
}

fn build_runtime_fn(e: &Expr) -> WalkResult<RuntimeFn> {
    let (name, args) = as_call(e)?;
    if name != "RuntimeFn" {
        return Err(format!("expected RuntimeFn(...), got {}(...)", name));
    }
    expect_arity(name, args, 5)?;
    let params = as_array(arg(args, 1)?)?
        .iter()
        .map(|p| build_runtime_param(&p.node))
        .collect::<WalkResult<Vec<_>>>()?;
    Ok(RuntimeFn {
        c_name: as_string(arg(args, 0)?)?,
        params,
        ret: build_c_runtime_type(arg(args, 2)?)?,
        side_effects: build_side_effects(arg(args, 3)?)?,
        returns_fresh: as_bool(arg(args, 4)?)?,
    })
}

// ── Tests ───────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// End-to-end: the embedded `resources.gg` loads, the schema version
    /// matches, and the full table lands with the expected shapes.
    /// Spot-checks cover the spike entries (which still lead the file) and
    /// a sample of the mechanical fill-in (item 8 of the resources.toml plan).
    #[test]
    fn resources_load_clean() {
        let t = table();

        assert_eq!(t.schema_version, SCHEMA_VERSION_EXPECTED);
        assert_eq!(t.resources.len(), 38, "expected 38 ResourceEntry rows after fill-in (Set__/HashSet__ split into two typed-kind entries)");
        assert_eq!(t.runtime_fns.len(), 299, "expected 299 RuntimeFn rows after fill-in");

        // Spot-check: GorgetString multi-alias Exact entry.
        let s = t.lookup("GorgetString").expect("GorgetString missing");
        assert_eq!(s.runtime_name, "GorgetString");
        assert_eq!(s.size_bytes, 32);
        assert_eq!(s.copy_semantics, CopySemantics::Resource);
        assert_eq!(s.collection_kind, CollectionKind::NotCollection);
        assert!(s.materialize_fn.is_some(), "GorgetString must carry materialize_fn");
        // Aliases hit the same payload.
        let s2 = t.lookup("Str").expect("Str alias missing");
        assert_eq!(s2.runtime_name, "GorgetString");

        // Spot-check: Vector__T prefix entry; CkVector, typed_constructor=true.
        let v = t.lookup("Vector__int64_t").expect("Vector__ prefix missing");
        assert_eq!(v.runtime_name, "GorgetArray");
        assert_eq!(v.collection_kind, CollectionKind::Vector);
        assert!(v.is_typed_constructor);

        // Spot-check: Deque__T and Heap__T are distinct kinds now.
        let d = t.lookup("Deque__String").expect("Deque__ prefix missing");
        assert_eq!(d.collection_kind, CollectionKind::Deque);
        let h = t.lookup("Heap__int64_t").expect("Heap__ prefix missing");
        assert_eq!(h.collection_kind, CollectionKind::Heap);
        assert!(!h.is_typed_constructor, "Heap__ uses factory shape");

        // Spot-check: Box__T = regular box; bare "Box" = trait box.
        let b = t.lookup("Box__int64_t").expect("Box__ missing");
        assert_eq!(b.box_kind, BoxKind::RegularBox);
        let bt = t.lookup("Box").expect("bare Box missing");
        assert_eq!(bt.box_kind, BoxKind::TraitBox);

        // Spot-check: Mutex__T is opaque + ref-counted + carries method_prefix.
        let m = t.lookup("Mutex__int64_t").expect("Mutex__ missing");
        assert!(m.opaque_handle);
        assert_eq!(m.copy_semantics, CopySemantics::RefCounted);
        assert_eq!(m.method_prefix.as_deref(), Some("gorget_mutex"));
        assert_eq!(m.c_typedef_name.as_deref(), Some("GorgetMutex*"));
        assert!(m.clone_fn.is_none(), "Mutex doesn't deep-clone");

        // Spot-check: c_typedef_name carries the C-typedef target for the
        // ref-counted-handle + guard families (item 7c). Each entry's
        // C typedef target diverges from its `runtime_name`, so the field
        // drives the wrapper-typedef emission in emit_wrapper_typedef.
        let chan = t.lookup("Channel__int64_t").expect("Channel__ missing");
        assert_eq!(chan.c_typedef_name.as_deref(), Some("GorgetChannel*"));
        let sh = t.lookup("Shared__int64_t").expect("Shared__ missing");
        assert_eq!(sh.c_typedef_name.as_deref(), Some("GorgetShared*"));
        let wk = t.lookup("Weak__int64_t").expect("Weak__ missing");
        assert_eq!(wk.c_typedef_name.as_deref(), Some("GorgetShared*"));
        let rw = t.lookup("RWLock__int64_t").expect("RWLock__ missing");
        assert_eq!(rw.c_typedef_name.as_deref(), Some("GorgetRWLock*"));
        let g = t.lookup("Guard__int64_t").expect("Guard__ missing");
        assert_eq!(g.c_typedef_name.as_deref(), Some("gorget_guard_t"));
        let rg = t.lookup("ReadGuard__int64_t").expect("ReadGuard__ missing");
        assert_eq!(rg.c_typedef_name.as_deref(), Some("gorget_read_guard_t"));
        let wg = t.lookup("WriteGuard__int64_t").expect("WriteGuard__ missing");
        assert_eq!(wg.c_typedef_name.as_deref(), Some("gorget_write_guard_t"));

        // Spot-check: rows that haven't diverged leave c_typedef_name=None
        // (the lookup falls back to runtime_name).
        assert!(v.c_typedef_name.is_none(), "Vector__ should leave c_typedef_name=None");
        assert!(s.c_typedef_name.is_none(), "GorgetString should leave c_typedef_name=None");

        // Spot-check: Dict alias cluster hits all three names.
        for k in &["Dict__int64_t__String", "HashMap__int64_t__String", "GorgetDict__int64_t__String"] {
            let entry = t.lookup(k).unwrap_or_else(|| panic!("Dict alias {} missing", k));
            assert_eq!(entry.collection_kind, CollectionKind::Dict);
        }

        // Unknown name returns None.
        assert!(t.lookup("not_a_real_type").is_none());

        // Runtime fn spot-checks: signature + side effects + returns_fresh.
        let cat = t.runtime_fn("gorget_str_cat").expect("gorget_str_cat missing");
        assert_eq!(cat.params.len(), 2);
        assert_eq!(cat.params[0].ty, CRuntimeType::Str);
        assert_eq!(cat.params[0].abi, AbiKind::GorgetString);
        assert_eq!(cat.ret, CRuntimeType::Str);
        assert_eq!(cat.side_effects, SideEffects::Allocates);
        assert!(cat.returns_fresh);

        let push = t.runtime_fn("gorget_array_push").expect("gorget_array_push missing");
        assert_eq!(push.params.len(), 2);
        assert_eq!(push.params[1].abi, AbiKind::VoidElem);
        assert_eq!(push.ret, CRuntimeType::Void);
        assert_eq!(push.side_effects, SideEffects::Mutates);
        assert!(!push.returns_fresh);

        let sqrt = t.runtime_fn("gorget_sqrt").expect("gorget_sqrt missing");
        assert_eq!(sqrt.side_effects, SideEffects::Pure);

        let lock = t.runtime_fn("gorget_mutex_lock").expect("gorget_mutex_lock missing");
        assert_eq!(lock.side_effects, SideEffects::Concurrent);

        // Unknown runtime fn returns None.
        assert!(t.runtime_fn("not_a_runtime_fn").is_none());
    }
}
