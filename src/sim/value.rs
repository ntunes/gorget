use std::rc::Rc;
use std::cell::RefCell;

/// A runtime value in the GIR interpreter.
#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Bool(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    /// Gorget `char` — a Unicode codepoint stored as u32.
    Char(u32),
    /// Gorget `str` — an immutable UTF-8 string view.
    Str(SimStr),
    /// Gorget `String` — owned heap string (GorgetString).
    String(SimString),
    /// Gorget `cstr` — a bare C string pointer (const char*).
    CStr(Rc<std::string::String>),
    /// Named struct with ordered fields.
    Struct { type_name: std::string::String, fields: Vec<Value> },
    /// Enum with tag (variant index), variant name, and payload fields.
    Enum { type_name: std::string::String, tag: i64, variant: std::string::String, fields: Vec<Value> },
    /// Tuple with ordered elements.
    Tuple(Vec<Value>),
    /// Reference to a named function.
    FuncRef(std::string::String),
    /// Immutable pointer (index into heap).
    Ptr(usize),
    /// Mutable pointer (index into heap).
    MutPtr(usize),
    /// Transparent heap-backed local (created by BorrowMut on a local).
    /// Reading/writing this value transparently goes through heap[addr].
    Ref(usize),
    /// Null pointer / null value.
    Null,
    /// Dynamic array (Gorget Vector[T] / GorgetArray). Ref-counted for cheap sharing.
    Array(SimArray),
    /// Ordered dict (Gorget Dict[K,V]).
    Dict(SimDict),
}

/// An immutable UTF-8 string view (Gorget `str`).
/// Backed by a shared buffer with byte offset and length.
#[derive(Debug, Clone)]
pub struct SimStr {
    pub data: Rc<Vec<u8>>,
    pub start: usize,
    pub len: usize,
}

impl SimStr {
    pub fn from_str(s: &str) -> Self {
        let bytes = s.as_bytes().to_vec();
        let len = bytes.len();
        Self { data: Rc::new(bytes), start: 0, len }
    }

    pub fn from_string(s: std::string::String) -> Self {
        let bytes = s.into_bytes();
        let len = bytes.len();
        Self { data: Rc::new(bytes), start: 0, len }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.data[self.start..self.start + self.len]
    }

    pub fn as_str(&self) -> &str {
        std::str::from_utf8(self.as_bytes()).unwrap_or("")
    }

    pub fn byte_len(&self) -> usize {
        self.len
    }

    pub fn codepoint_count(&self) -> usize {
        self.as_str().chars().count()
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}

/// An owned UTF-8 string (Gorget `String` / GorgetString).
/// `cap` tracks the requested initial capacity (mirrors GorgetString.cap in C).
#[derive(Debug, Clone)]
pub struct SimString {
    pub data: Vec<u8>,
    /// Requested capacity (preserved across clones, mirrors GorgetString.cap).
    pub cap: usize,
}

impl SimString {
    pub fn new() -> Self {
        Self { data: Vec::new(), cap: 0 }
    }

    pub fn from_str(s: &str) -> Self {
        let bytes = s.as_bytes().to_vec();
        let cap = bytes.len();
        Self { data: bytes, cap }
    }

    pub fn from_string(s: std::string::String) -> Self {
        let bytes = s.into_bytes();
        let cap = bytes.len();
        Self { data: bytes, cap }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self { data: Vec::with_capacity(cap), cap }
    }

    pub fn as_str(&self) -> &str {
        std::str::from_utf8(&self.data).unwrap_or("")
    }

    pub fn byte_len(&self) -> usize {
        self.data.len()
    }

    pub fn capacity(&self) -> usize {
        self.cap.max(self.data.len())
    }

    pub fn to_sim_str(&self) -> SimStr {
        SimStr::from_str(self.as_str())
    }
}

/// A dynamic array (Gorget `Vector[T]` / `GorgetArray`).
/// Ref-counted so cloning is cheap and mutations are shared across all handles.
#[derive(Debug, Clone)]
pub struct SimArray {
    pub data: Rc<RefCell<Vec<Value>>>,
    /// Type name, e.g. "Vector__int64_t"
    pub type_name: std::string::String,
    /// Tracking allocator ID bound at array construction (for TrackingAllocator support).
    pub alloc_id: Rc<std::cell::Cell<Option<usize>>>,
}

impl SimArray {
    pub fn new(type_name: impl Into<std::string::String>) -> Self {
        Self {
            data: Rc::new(RefCell::new(Vec::new())),
            type_name: type_name.into(),
            alloc_id: Rc::new(std::cell::Cell::new(None)),
        }
    }

    pub fn len(&self) -> usize { self.data.borrow().len() }
    pub fn is_empty(&self) -> bool { self.len() == 0 }
    pub fn push(&self, v: Value) { self.data.borrow_mut().push(v); }
    pub fn get(&self, idx: usize) -> Option<Value> { self.data.borrow().get(idx).cloned() }
    pub fn set(&self, idx: usize, v: Value) {
        let mut d = self.data.borrow_mut();
        if idx < d.len() { d[idx] = v; }
    }
    pub fn pop(&self) -> Option<Value> { self.data.borrow_mut().pop() }
    pub fn remove(&self, idx: usize) -> Option<Value> {
        let mut d = self.data.borrow_mut();
        if idx < d.len() { Some(d.remove(idx)) } else { None }
    }
    pub fn clear(&self) { self.data.borrow_mut().clear(); }
    pub fn insert(&self, idx: usize, v: Value) {
        let mut d = self.data.borrow_mut();
        let idx = idx.min(d.len());
        d.insert(idx, v);
    }
    pub fn contains(&self, v: &Value) -> bool {
        self.data.borrow().iter().any(|x| values_equal(x, v))
    }
    pub fn index_of(&self, v: &Value) -> Option<usize> {
        self.data.borrow().iter().position(|x| values_equal(x, v))
    }
    pub fn clone_deep(&self) -> Self {
        SimArray {
            data: Rc::new(RefCell::new(self.data.borrow().clone())),
            type_name: self.type_name.clone(),
            alloc_id: Rc::new(std::cell::Cell::new(self.alloc_id.get())),
        }
    }
    pub fn to_vec(&self) -> Vec<Value> { self.data.borrow().clone() }

    /// Extract the inner element type name (e.g. "Vector__int64_t" → "int64_t").
    pub fn elem_type_name(&self) -> &str {
        self.type_name.strip_prefix("Vector__").unwrap_or("int64_t")
    }
}

/// A simple ordered dict (Gorget `Dict[K,V]` / `HashMap[K,V]`).
/// Uses a Vec for insertion-order preservation.
#[derive(Debug, Clone)]
pub struct SimDict {
    pub entries: Rc<RefCell<Vec<(Value, Value)>>>,
    pub type_name: std::string::String,
}

impl SimDict {
    pub fn new(type_name: impl Into<std::string::String>) -> Self {
        Self { entries: Rc::new(RefCell::new(Vec::new())), type_name: type_name.into() }
    }

    pub fn len(&self) -> usize { self.entries.borrow().len() }
    pub fn is_empty(&self) -> bool { self.len() == 0 }

    pub fn get(&self, key: &Value) -> Option<Value> {
        self.entries.borrow().iter()
            .find(|(k, _)| values_equal(k, key))
            .map(|(_, v)| v.clone())
    }

    pub fn set(&self, key: Value, val: Value) {
        let mut e = self.entries.borrow_mut();
        if let Some(entry) = e.iter_mut().find(|(k, _)| values_equal(k, &key)) {
            entry.1 = val;
        } else {
            e.push((key, val));
        }
    }

    pub fn contains(&self, key: &Value) -> bool {
        self.entries.borrow().iter().any(|(k, _)| values_equal(k, key))
    }

    pub fn remove(&self, key: &Value) -> Option<Value> {
        let mut e = self.entries.borrow_mut();
        if let Some(pos) = e.iter().position(|(k, _)| values_equal(k, key)) {
            Some(e.remove(pos).1)
        } else {
            None
        }
    }

    pub fn keys(&self) -> Vec<Value> {
        self.entries.borrow().iter().map(|(k, _)| k.clone()).collect()
    }

    pub fn values(&self) -> Vec<Value> {
        self.entries.borrow().iter().map(|(_, v)| v.clone()).collect()
    }

    pub fn items(&self) -> Vec<(Value, Value)> {
        self.entries.borrow().clone()
    }

    pub fn key_at(&self, idx: usize) -> Option<Value> {
        self.entries.borrow().get(idx).map(|(k, _)| k.clone())
    }

    pub fn val_at(&self, idx: usize) -> Option<Value> {
        self.entries.borrow().get(idx).map(|(_, v)| v.clone())
    }

    pub fn clear(&self) { self.entries.borrow_mut().clear(); }

    pub fn clone_deep(&self) -> Self {
        SimDict { entries: Rc::new(RefCell::new(self.entries.borrow().clone())), type_name: self.type_name.clone() }
    }
}

/// Structural equality comparison for Values (used by SimArray/SimDict).
pub fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Unit, Value::Unit) | (Value::Null, Value::Null) => true,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::I64(x), Value::I64(y)) => x == y,
        (Value::I32(x), Value::I32(y)) => x == y,
        (Value::I8(x), Value::I8(y)) => x == y,
        (Value::I16(x), Value::I16(y)) => x == y,
        (Value::U64(x), Value::U64(y)) => x == y,
        (Value::U32(x), Value::U32(y)) => x == y,
        (Value::U8(x), Value::U8(y)) => x == y,
        (Value::U16(x), Value::U16(y)) => x == y,
        (Value::F32(x), Value::F32(y)) => x == y,
        (Value::F64(x), Value::F64(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x.as_str() == y.as_str(),
        (Value::String(x), Value::String(y)) => x.as_str() == y.as_str(),
        (Value::CStr(x), Value::CStr(y)) => x.as_str() == y.as_str(),
        // Cross-type integer comparison (e.g. I64 vs I32)
        _ if a.is_integer() && b.is_integer() => a.as_i64() == b.as_i64(),
        _ if a.is_string() && b.is_string() => a.as_str_content() == b.as_str_content(),
        _ => false,
    }
}

impl Value {
    /// Interpret value as a boolean (for conditionals).
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::I8(n) => *n != 0,
            Value::I16(n) => *n != 0,
            Value::I32(n) => *n != 0,
            Value::I64(n) => *n != 0,
            Value::U8(n) => *n != 0,
            Value::U16(n) => *n != 0,
            Value::U32(n) => *n != 0,
            Value::U64(n) => *n != 0,
            Value::Char(c) => *c != 0,
            Value::Ptr(a) | Value::MutPtr(a) => *a != 0,
            Value::Null => false,
            // Arrays/Dicts: truthy when non-empty
            Value::Array(a) => !a.is_empty(),
            Value::Dict(d) => !d.is_empty(),
            // Unit/other: treat as false rather than panicking
            Value::Unit => false,
            Value::Ref(a) => *a != 0,
            Value::String(s) => !s.as_str().is_empty(),
            Value::Str(s) => !s.is_empty(),
            Value::Tuple(elems) => !elems.is_empty(),
            Value::Struct { fields, .. } => !fields.is_empty(),
            Value::Enum { tag, .. } => *tag != 0,
            Value::FuncRef(_) | Value::CStr(_) => true,
            _ => panic!("as_bool on {:?}", std::mem::discriminant(self)),
        }
    }

    /// Interpret value as i64 (for integer operations and switch tags).
    /// Returns 0 for non-integer types to avoid panics from unimplemented functions returning Unit.
    pub fn as_i64(&self) -> i64 {
        match self {
            Value::Bool(b) => *b as i64,
            Value::I8(n) => *n as i64,
            Value::I16(n) => *n as i64,
            Value::I32(n) => *n as i64,
            Value::I64(n) => *n,
            Value::U8(n) => *n as i64,
            Value::U16(n) => *n as i64,
            Value::U32(n) => *n as i64,
            Value::U64(n) => *n as i64,
            Value::F32(f) => *f as i64,
            Value::F64(f) => *f as i64,
            Value::Char(c) => *c as i64,
            Value::Ptr(a) | Value::MutPtr(a) | Value::Ref(a) => *a as i64,
            Value::Null => 0,
            Value::Array(a) => a.len() as i64,
            Value::Dict(d) => d.len() as i64,
            // Unit and Enum: return 0 (enum tag) — prevents panics from unimpl functions.
            Value::Unit => 0,
            Value::Enum { tag, .. } => *tag,
            // String: return 0 (prevents panic; do_printf handles Str directly)
            Value::Str(s) => s.len as i64,
            Value::String(s) => s.byte_len() as i64,
            Value::CStr(s) => s.len() as i64,
            _ => 0, // Struct, Tuple, FuncRef — return 0
        }
    }

    /// Interpret value as u64 (for unsigned operations).
    pub fn as_u64(&self) -> u64 {
        match self {
            Value::Bool(b) => *b as u64,
            Value::I8(n) => *n as u64,
            Value::I16(n) => *n as u64,
            Value::I32(n) => *n as u64,
            Value::I64(n) => *n as u64,
            Value::U8(n) => *n as u64,
            Value::U16(n) => *n as u64,
            Value::U32(n) => *n as u64,
            Value::U64(n) => *n,
            Value::F32(f) => *f as u64,
            Value::F64(f) => *f as u64,
            Value::Char(c) => *c as u64,
            Value::Ptr(a) | Value::MutPtr(a) | Value::Ref(a) => *a as u64,
            Value::Null | Value::Unit => 0,
            Value::Array(a) => a.len() as u64,
            Value::Dict(d) => d.len() as u64,
            Value::Enum { tag, .. } => *tag as u64,
            _ => 0, // graceful fallback
        }
    }

    /// Interpret value as f64 (for float operations).
    pub fn as_f64(&self) -> f64 {
        match self {
            Value::F32(f) => *f as f64,
            Value::F64(f) => *f,
            Value::I8(n) => *n as f64,
            Value::I16(n) => *n as f64,
            Value::I32(n) => *n as f64,
            Value::I64(n) => *n as f64,
            Value::U8(n) => *n as f64,
            Value::U16(n) => *n as f64,
            Value::U32(n) => *n as f64,
            Value::U64(n) => *n as f64,
            _ => 0.0, // graceful fallback
        }
    }

    /// Get the string content (for Str, String, CStr values).
    pub fn as_str_content(&self) -> &str {
        match self {
            Value::Str(s) => s.as_str(),
            Value::String(s) => s.as_str(),
            Value::CStr(s) => s.as_str(),
            _ => panic!("as_str_content on {:?}", std::mem::discriminant(self)),
        }
    }

    /// Get string content as an owned String, or empty string for non-string values.
    pub fn as_str_lossy(&self) -> String {
        match self {
            Value::Str(s) => s.as_str().to_owned(),
            Value::String(s) => s.as_str().to_owned(),
            Value::CStr(s) => s.as_str().to_owned(),
            _ => String::new(),
        }
    }

    /// Convert to a Unicode char (for Char values).
    pub fn as_char(&self) -> char {
        match self {
            Value::Char(c) => char::from_u32(*c).unwrap_or('\0'),
            Value::I8(n) => char::from_u32(*n as u32).unwrap_or('\0'),
            Value::I16(n) => char::from_u32(*n as u32).unwrap_or('\0'),
            Value::I32(n) => char::from_u32(*n as u32).unwrap_or('\0'),
            Value::I64(n) => char::from_u32(*n as u32).unwrap_or('\0'),
            Value::U8(n) => *n as char,
            Value::U32(n) => char::from_u32(*n).unwrap_or('\0'),
            Value::U64(n) => char::from_u32(*n as u32).unwrap_or('\0'),
            _ => '\0',
        }
    }

    /// Convert to a SimStr view (works for Str, String, CStr).
    pub fn to_sim_str(&self) -> SimStr {
        match self {
            Value::Str(s) => s.clone(),
            Value::String(s) => s.to_sim_str(),
            Value::CStr(s) => SimStr::from_str(s),
            _ => panic!("to_sim_str on {:?}", std::mem::discriminant(self)),
        }
    }

    /// Try to convert to SimStr, returning None for non-string values.
    pub fn try_to_sim_str(&self) -> Option<SimStr> {
        match self {
            Value::Str(s) => Some(s.clone()),
            Value::String(s) => Some(s.to_sim_str()),
            Value::CStr(s) => Some(SimStr::from_str(s)),
            _ => None,
        }
    }

    /// Get the type name for error messages.
    pub fn type_name(&self) -> &str {
        match self {
            Value::Unit => "unit",
            Value::Bool(_) => "bool",
            Value::I8(_) => "i8",
            Value::I16(_) => "i16",
            Value::I32(_) => "i32",
            Value::I64(_) => "i64",
            Value::U8(_) => "u8",
            Value::U16(_) => "u16",
            Value::U32(_) => "u32",
            Value::U64(_) => "u64",
            Value::F32(_) => "f32",
            Value::F64(_) => "f64",
            Value::Char(_) => "char",
            Value::Str(_) => "str",
            Value::String(_) => "String",
            Value::CStr(_) => "cstr",
            Value::Struct { type_name, .. } => type_name.as_str(),
            Value::Enum { type_name, .. } => type_name.as_str(),
            Value::Tuple(_) => "tuple",
            Value::FuncRef(_) => "funcref",
            Value::Ptr(_) => "ptr",
            Value::MutPtr(_) => "mutptr",
            Value::Ref(_) => "ref",
            Value::Null => "null",
            Value::Array(a) => a.type_name.as_str(),
            Value::Dict(d) => d.type_name.as_str(),
        }
    }

    /// Check if this is a numeric integer type.
    pub fn is_integer(&self) -> bool {
        matches!(self, Value::Bool(_) | Value::I8(_) | Value::I16(_) | Value::I32(_) | Value::I64(_)
            | Value::U8(_) | Value::U16(_) | Value::U32(_) | Value::U64(_) | Value::Char(_))
    }

    /// Check if this is a float type.
    pub fn is_float(&self) -> bool {
        matches!(self, Value::F32(_) | Value::F64(_))
    }

    /// Check if this is a string-like type.
    pub fn is_string(&self) -> bool {
        matches!(self, Value::Str(_) | Value::String(_) | Value::CStr(_))
    }

    /// Get the enum tag (variant index).
    pub fn enum_tag(&self) -> i64 {
        match self {
            Value::Enum { tag, .. } => *tag,
            _ => panic!("enum_tag on {:?}", std::mem::discriminant(self)),
        }
    }

    /// Create a zero value for the given GIR type.
    pub fn zero_for_type(type_id: crate::ir::types::TypeId, registry: &crate::ir::types::TypeRegistry) -> Self {
        use crate::ir::types::*;
        if type_id == BOOL_TYPE { return Value::Bool(false); }
        if type_id == I8_TYPE { return Value::I8(0); }
        if type_id == I16_TYPE { return Value::I16(0); }
        if type_id == I32_TYPE { return Value::I32(0); }
        if type_id == I64_TYPE { return Value::I64(0); }
        if type_id == U8_TYPE { return Value::U8(0); }
        if type_id == U16_TYPE { return Value::U16(0); }
        if type_id == U32_TYPE { return Value::U32(0); }
        if type_id == U64_TYPE { return Value::U64(0); }
        if type_id == F32_TYPE { return Value::F32(0.0); }
        if type_id == F64_TYPE { return Value::F64(0.0); }
        if type_id == UNIT_TYPE { return Value::Unit; }
        if type_id == CHAR_TYPE { return Value::Char(0); }
        match registry.get(type_id) {
            Some(GirType::Named(name)) => {
                // Try to create a zero value from the type def
                if let Some(def) = registry.get_type_def(name) {
                    match &def.kind {
                        TypeDefKind::Struct(s) => {
                            let fields = s.fields.iter().map(|f| Value::zero_for_type(f.type_id, registry)).collect();
                            Value::Struct { type_name: name.clone(), fields }
                        }
                        TypeDefKind::Enum(e) => {
                            if e.variants.is_empty() {
                                Value::Enum { type_name: name.clone(), tag: 0, variant: String::new(), fields: vec![] }
                            } else {
                                let v = &e.variants[0];
                                let fields = v.fields.iter().map(|f| Value::zero_for_type(f.type_id, registry)).collect();
                                Value::Enum { type_name: name.clone(), tag: 0, variant: v.name.clone(), fields }
                            }
                        }
                        TypeDefKind::Alias(inner) => Value::zero_for_type(*inner, registry),
                    }
                } else {
                    // Unknown named type — treat as str or GorgetString based on name
                    match name.as_str() {
                        "Str" => Value::Str(SimStr::from_str("")),
                        "GorgetString" => Value::String(SimString::new()),
                        n if n.starts_with("Vector__") || n == "GorgetArray" => {
                            Value::Array(SimArray::new(n))
                        }
                        n if n.starts_with("Dict__") || n.starts_with("HashMap__") || n == "GorgetMap" || n == "GorgetDict" => {
                            Value::Dict(SimDict::new(n))
                        }
                        _ => Value::I64(0), // fallback
                    }
                }
            }
            Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_)) => Value::Ptr(0),
            Some(GirType::FnPtr { .. }) => Value::Null,
            _ => Value::I64(0), // fallback for unknown types
        }
    }
}
