//! String pooling and interning implementation.
//!
//! This module provides a `StringPool` that implements efficient string interning,
//! where identical strings are stored only once and referenced by unique IDs.
//!
//! This implementation is backed by the `string_interner` crate.
//! See <https://github.com/Robbepop/string-interner> for more details.
//!
//! # Example
//!
//! ```rust
//! use index::string_pool::{StringPool, PoolId, SymbolId};
//!
//! // Define custom ID types directly
//! #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
//! struct NameId(SymbolId);
//!
//! impl PoolId for NameId {
//!     fn from_symbol(symbol: SymbolId) -> Self {
//!         Self(symbol)
//!     }
//!
//!     fn to_symbol(&self) -> SymbolId {
//!         self.0
//!     }
//! }
//!
//! let mut name_pool: StringPool<NameId> = StringPool::new();
//!
//! // Add strings to pools
//! let id1 = name_pool.add("Foo".to_string());
//! let id2 = name_pool.add("Bar".to_string());
//! let id3 = name_pool.add("Foo".to_string());
//!
//! assert_ne!(id1, id2); // Different names, different IDs
//! assert_eq!(id1, id3); // Same name, same ID (string interning)
//!
//! // Retrieve names by ID (returns owned String)
//! assert_eq!(name_pool.get(id1), Some("Foo".to_string()));
//! assert_eq!(name_pool.get(id2), Some("Bar".to_string()));
//! ```

use std::marker::PhantomData;
use string_interner::{StringInterner, backend::BufferBackend, symbol::SymbolU32};

/// An alias to `string_interner::symbol::SymbolU32`
///
/// This is the type of the symbols used by the string interner.
/// It is used to identify unique strings in the pool.
///
/// This is a type alias to avoid having to import the `symbol` module everywhere.
pub type SymbolId = SymbolU32;

/// A trait for types that can serve as pool identifiers.
///
/// Types implementing this trait should be new type wrappers around `SymbolId`
/// to provide type safety while maintaining efficiency.
///
/// # Example
///
/// ```rust
/// use index::string_pool::PoolId;
///
/// #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
/// struct NameId(SymbolId);
///
/// impl PoolId for NameId {
///     fn from_symbol(symbol: SymbolId) -> Self {
///         Self(symbol)
///     }
///
///     fn to_symbol(&self) -> SymbolId {
///         self.0
///     }
/// }
/// ```
pub trait PoolId: Copy + Clone + PartialEq + Eq + std::hash::Hash + std::fmt::Debug {
    /// Creates a new pool identifier from a `SymbolId`.
    fn from_symbol(symbol: SymbolId) -> Self;

    /// Converts this pool identifier to a `SymbolId`.
    fn to_symbol(&self) -> SymbolId;

    /// Converts this pool identifier to a `String`.
    fn to_string(&self, pool: &StringPool<Self>) -> String {
        let symbol = self.to_symbol();
        pool.backend.resolve(symbol).unwrap().to_string()
    }
}

/// A string pool implementation with efficient string interning.
///
/// # Example
///
/// ```rust
/// let mut name_pool: StringPool<NameId> = StringPool::new();
///
/// // Add strings to pools
/// let id1 = name_pool.add("Foo".to_string());
/// let id2 = name_pool.add("Bar".to_string());
/// let id3 = name_pool.add("Foo".to_string());
///
/// assert_ne!(id1, id2); // Different names, different IDs
/// assert_eq!(id1, id3); // Same name, same ID (string interning)
///
/// // Retrieve names by ID (returns owned String)
/// assert_eq!(name_pool.get(id1), Some("Foo".to_string()));
/// assert_eq!(name_pool.get(id2), Some("Bar".to_string()));
/// ```
#[derive(Debug)]
pub struct StringPool<I: PoolId> {
    backend: StringInterner<BufferBackend<SymbolId>>,
    _marker: PhantomData<I>,
}

impl<I: PoolId> StringPool<I> {
    /// Creates a new empty string pool.
    #[must_use]
    pub fn new() -> Self {
        Self {
            backend: StringInterner::<BufferBackend<SymbolId>>::new(),
            _marker: PhantomData,
        }
    }

    /// Adds a string to the pool and returns its ID.
    ///
    /// If the string already exists in the pool, returns the existing ID.
    /// Otherwise, adds the string and returns a new ID. This implements
    /// string interning - identical strings share the same ID.
    ///
    /// # Arguments
    ///
    /// * `string` - The string to add to the pool
    ///
    /// # Returns
    ///
    /// The ID of the string in the pool
    pub fn add(&mut self, string: String) -> I {
        let symbol: SymbolId = self.backend.get_or_intern(string);
        I::from_symbol(symbol)
    }

    /// Retrieves a string from the pool by its ID.
    ///
    /// # Arguments
    ///
    /// * `id` - The ID of the string to retrieve
    ///
    /// # Returns
    ///
    /// `Some(String)` if the ID exists in the pool, `None` otherwise
    pub fn get(&self, id: I) -> Option<String> {
        let symbol = id.to_symbol();
        self.backend.resolve(symbol).map(std::string::ToString::to_string)
    }

    /// Returns the number of unique strings in the pool.
    #[must_use]
    pub fn size(&self) -> usize {
        self.backend.len()
    }

    /// Returns `true` if the pool contains no strings.
    #[must_use]
    pub fn empty(&self) -> bool {
        self.backend.is_empty()
    }
}

impl<I: PoolId> Default for StringPool<I> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use string_interner::Symbol;

    // Define a test ID type
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    struct TestId(SymbolId);

    impl PoolId for TestId {
        fn from_symbol(symbol: SymbolId) -> Self {
            Self(symbol)
        }

        fn to_symbol(&self) -> SymbolId {
            self.0
        }
    }

    #[test]
    fn pool_new_creates_empty_pool() {
        let pool: StringPool<TestId> = StringPool::new();
        assert_eq!(pool.size(), 0);
        assert!(pool.empty());
    }

    #[test]
    fn pool_add_single_string() {
        let mut pool: StringPool<TestId> = StringPool::new();
        let id = pool.add("hello".to_string());

        assert_eq!(pool.size(), 1);
        assert_eq!(pool.get(id), Some("hello".to_string()));
        assert!(!pool.empty());
    }

    #[test]
    fn pool_add_multiple_strings() {
        let mut pool: StringPool<TestId> = StringPool::new();

        let id1 = pool.add("hello".to_string());
        let id2 = pool.add("world".to_string());
        let id3 = pool.add("rust".to_string());

        assert_eq!(pool.size(), 3);
        assert!(!pool.empty());
        assert_eq!(pool.get(id1), Some("hello".to_string()));
        assert_eq!(pool.get(id2), Some("world".to_string()));
        assert_eq!(pool.get(id3), Some("rust".to_string()));
    }

    #[test]
    fn pool_string_interning_behavior() {
        let mut pool: StringPool<TestId> = StringPool::new();

        // Add the same string twice
        let id1 = pool.add("hello".to_string());
        let id2 = pool.add("hello".to_string());

        // Should return the same ID due to string interning
        assert_eq!(id1, id2);
        assert_eq!(pool.size(), 1); // Should still be 1 item
        assert_eq!(pool.get(id1), Some("hello".to_string()));
        assert_eq!(pool.get(id2), Some("hello".to_string()));
    }

    #[test]
    fn pool_different_strings_different_ids() {
        let mut pool: StringPool<TestId> = StringPool::new();

        let id1 = pool.add("hello".to_string());
        let id2 = pool.add("world".to_string());

        assert_ne!(id1, id2);
        assert_eq!(pool.size(), 2);
    }

    #[test]
    fn pool_get_invalid_id_returns_none() {
        let pool: StringPool<TestId> = StringPool::new();

        // Try to get with an ID that doesn't exist
        let invalid_id = TestId::from_symbol(SymbolId::try_from_usize(999).unwrap());
        assert_eq!(pool.get(invalid_id), None);
    }

    #[test]
    fn pool_get_out_of_bounds_id_returns_none() {
        let mut pool: StringPool<TestId> = StringPool::new();

        // Add one item
        pool.add("hello".to_string());

        // Try to get with an ID that's out of bounds
        let out_of_bounds_id = TestId::from_symbol(SymbolId::try_from_usize(100).unwrap());
        assert_eq!(pool.get(out_of_bounds_id), None);
    }

    #[test]
    fn pool_empty_string() {
        let mut pool: StringPool<TestId> = StringPool::new();

        let id = pool.add(String::new());
        assert_eq!(pool.size(), 1);
        assert_eq!(pool.get(id), Some(String::new()));
    }

    #[test]
    fn pool_unicode_strings() {
        let mut pool: StringPool<TestId> = StringPool::new();

        let unicode_str = "Hello, 世界! 🦀";
        let id = pool.add(unicode_str.to_string());

        assert_eq!(pool.size(), 1);
        assert_eq!(pool.get(id), Some(unicode_str.to_string()));
    }

    #[test]
    fn pool_long_string() {
        let mut pool: StringPool<TestId> = StringPool::new();

        let long_string = "a".repeat(1000);
        let id = pool.add(long_string.clone());

        assert_eq!(pool.size(), 1);
        assert_eq!(pool.get(id), Some(long_string));
    }

    #[test]
    fn pool_strings_with_whitespace() {
        let mut pool: StringPool<TestId> = StringPool::new();

        let strings = vec!["  hello  ", "hello", " hello ", "\thello\n", "hello\r\n"];

        let mut ids = Vec::new();
        for s in &strings {
            ids.push(pool.add((*s).to_string()));
        }

        // All strings are different due to whitespace
        assert_eq!(pool.size(), 5);

        // Verify each string is stored correctly
        for (i, s) in strings.iter().enumerate() {
            assert_eq!(pool.get(ids[i]), Some((*s).to_string()));
        }
    }

    #[test]
    fn pool_case_sensitivity() {
        let mut pool: StringPool<TestId> = StringPool::new();

        let id1 = pool.add("Hello".to_string());
        let id2 = pool.add("hello".to_string());
        let id3 = pool.add("HELLO".to_string());

        // Should be treated as different strings
        assert_ne!(id1, id2);
        assert_ne!(id1, id3);
        assert_ne!(id2, id3);
        assert_eq!(pool.size(), 3);

        assert_eq!(pool.get(id1), Some("Hello".to_string()));
        assert_eq!(pool.get(id2), Some("hello".to_string()));
        assert_eq!(pool.get(id3), Some("HELLO".to_string()));
    }

    #[test]
    fn pool_multiple_interning_same_string() {
        let mut pool: StringPool<TestId> = StringPool::new();

        let strings = vec!["hello", "world", "hello", "rust", "world", "hello"];
        let mut ids = Vec::new();

        for s in strings {
            ids.push(pool.add(s.to_string()));
        }

        // Should only have 3 unique strings
        assert_eq!(pool.size(), 3);

        // Same strings should have same IDs
        assert_eq!(ids[0], ids[2]); // "hello"
        assert_eq!(ids[0], ids[5]); // "hello"
        assert_eq!(ids[1], ids[4]); // "world"

        // Different strings should have different IDs
        assert_ne!(ids[0], ids[1]); // "hello" vs "world"
        assert_ne!(ids[0], ids[3]); // "hello" vs "rust"
        assert_ne!(ids[1], ids[3]); // "world" vs "rust"
    }

    #[test]
    fn pool_stress_test() {
        let mut pool: StringPool<TestId> = StringPool::new();
        let mut ids = Vec::new();

        // Add 100 unique strings
        for i in 0..100 {
            let s = format!("string_{i}");
            let id = pool.add(s);
            ids.push(id);
        }

        assert_eq!(pool.size(), 100);

        // Verify all strings can be retrieved
        for (i, id) in ids.iter().enumerate() {
            let expected = format!("string_{i}");
            assert_eq!(pool.get(*id), Some(expected));
        }
    }

    #[test]
    fn pool_interning_with_duplicates_stress_test() {
        let mut pool: StringPool<TestId> = StringPool::new();
        let mut all_ids = Vec::new();

        // Add the same 10 strings 10 times each
        for _round in 0..10 {
            for i in 0..10 {
                let s = format!("string_{i}");
                let id = pool.add(s);
                all_ids.push((id, i));
            }
        }

        // Should only have 10 unique strings
        assert_eq!(pool.size(), 10);

        // All IDs for the same string should be equal
        for round in 0..10 {
            for i in 0..10 {
                let base_idx = i;
                let current_idx = round * 10 + i;
                assert_eq!(all_ids[base_idx].0, all_ids[current_idx].0);
            }
        }
    }
}
