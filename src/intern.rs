//! String interning for fast identifier comparison and reduced allocations.
//!
//! All source identifiers pass through the global interner at lex time.
//! `Symbol` is a lightweight handle (u32) that can be cheaply copied, compared,
//! and hashed. Use `Symbol::as_str()` to recover the original text.

use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fmt;

/// An interned string identifier. Cheap to copy, compare, and hash.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(u32);

impl Symbol {
    /// Resolve this symbol back to its string content.
    ///
    /// # Safety justification
    /// The interner's `Vec<String>` only grows — entries are never removed or
    /// modified. Each `String`'s heap data is stable across `Vec` reallocations
    /// (the Vec stores `(ptr, len, cap)` tuples; the pointed-to data doesn't move).
    /// The thread-local interner lives for the thread's duration, so the returned
    /// reference is valid for any practical lifetime.
    pub fn as_str(&self) -> &str {
        INTERNER.with(|i| {
            let interner = i.borrow();
            let s: &str = &interner.strings[self.0 as usize];
            // SAFETY: see above — the String's heap data is stable.
            unsafe { &*(s as *const str) }
        })
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Symbol({}: {:?})", self.0, self.as_str())
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl PartialEq<str> for Symbol {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<&str> for Symbol {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<Symbol> for str {
    fn eq(&self, other: &Symbol) -> bool {
        self == other.as_str()
    }
}

impl PartialEq<Symbol> for &str {
    fn eq(&self, other: &Symbol) -> bool {
        *self == other.as_str()
    }
}

// ── Interner ──────────────────────────────────────────────────

struct Interner {
    map: FxHashMap<String, Symbol>,
    strings: Vec<String>,
}

impl Interner {
    fn new() -> Self {
        Self {
            map: FxHashMap::default(),
            strings: Vec::new(),
        }
    }

    fn intern(&mut self, s: &str) -> Symbol {
        if let Some(&sym) = self.map.get(s) {
            return sym;
        }
        let sym = Symbol(self.strings.len() as u32);
        let owned = s.to_string();
        self.strings.push(owned.clone());
        self.map.insert(owned, sym);
        sym
    }
}

thread_local! {
    static INTERNER: RefCell<Interner> = RefCell::new(Interner::new());
}

/// Intern a string, returning a `Symbol` handle for fast comparison.
pub fn intern(s: &str) -> Symbol {
    INTERNER.with(|i| i.borrow_mut().intern(s))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intern_and_resolve() {
        let a = intern("hello");
        let b = intern("world");
        let c = intern("hello");

        assert_eq!(a, c);
        assert_ne!(a, b);
        assert_eq!(a.as_str(), "hello");
        assert_eq!(b.as_str(), "world");
    }

    #[test]
    fn display_and_partial_eq() {
        let sym = intern("foo");
        assert_eq!(format!("{sym}"), "foo");
        assert!(sym == "foo");
        assert!("foo" == sym);
        assert!(sym != "bar");
    }
}
