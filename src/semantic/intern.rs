use rustc_hash::FxHashMap;

/// Interned string identifier for fast comparison.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InternId(pub u32);

/// A simple string interner for fast name comparison.
pub struct Interner {
    map: FxHashMap<String, InternId>,
    strings: Vec<String>,
}

impl Interner {
    pub fn new() -> Self {
        Self {
            map: FxHashMap::default(),
            strings: Vec::new(),
        }
    }

    /// Intern a string, returning its unique ID.
    pub fn intern(&mut self, s: &str) -> InternId {
        if let Some(&id) = self.map.get(s) {
            return id;
        }
        let id = InternId(self.strings.len() as u32);
        self.strings.push(s.to_string());
        self.map.insert(s.to_string(), id);
        id
    }

    /// Look up the string for a given ID.
    pub fn resolve(&self, id: InternId) -> &str {
        &self.strings[id.0 as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intern_and_resolve() {
        let mut interner = Interner::new();
        let a = interner.intern("hello");
        let b = interner.intern("world");
        let c = interner.intern("hello");

        assert_eq!(a, c);
        assert_ne!(a, b);
        assert_eq!(interner.resolve(a), "hello");
        assert_eq!(interner.resolve(b), "world");
    }
}
