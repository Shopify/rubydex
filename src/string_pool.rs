//! String pooling and interning implementation.
//!
//! This module provides a `StringPool` that implements efficient string interning,
//! where identical strings are stored only once and referenced by unique IDs.
//!
//! This implementation is strongly inspired by the `string_interner` crate.
//! See <https://github.com/Robbepop/string-interner>.
//!
//! # Examples
//!
//! ```rust
//! use index::pool::{Pool, PoolId};
//! use index::string_pool::StringPool;
//!
//! // Define a custom ID type
//! #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
//! struct NameId(u32);
//!
//! impl PoolId for NameId {
//!     fn from_u32(id: u32) -> Self { NameId(id) }
//!     fn to_u32(&self) -> u32 { self.0 }
//! }
//!
//! let mut pool: StringPool<NameId> = StringPool::new();
//!
//! // Using the Pool trait methods (requires String allocation)
//! let id1 = pool.add("Foo".to_string());
//! let id2 = pool.add("Bar".to_string());
//! let id3 = pool.add("Foo".to_string());
//!
//! assert_ne!(id1, id2); // Different names, different IDs
//! assert_eq!(id1, id3); // Same name, same ID (string interning)
//!
//! // Retrieve names by ID (returns owned String)
//! assert_eq!(pool.get(&id1), Some("Foo".to_string()));
//! assert_eq!(pool.get(&id2), Some("Bar".to_string()));
//! ```

use core::fmt::Debug;
use core::marker::PhantomData;

use ahash::RandomState;
use hashbrown::HashMap;
use hashbrown::hash_map::RawEntryMut;

/// A trait for types that can serve as pool identifiers.
///
/// Pool identifiers are backed by u32 values for compact storage.
///
/// # Example
/// ```rust
/// struct MyPoolId(u32);
///
/// impl PoolId for MyPoolId {
///     fn from_u32(id: u32) -> Self {
///         MyPoolId(id)
///     }
///
///     fn to_u32(&self) -> u32 {
///         self.0
///     }
/// }
pub trait PoolId {
    /// Creates a new pool identifier from a u32 value.
    fn from_u32(id: u32) -> Self;

    /// Converts this pool identifier to a u32 value.
    fn to_u32(&self) -> u32;
}

/// A string pool implementation with efficient string interning.
///
/// # Implementation Details
///
/// The pool uses:
/// - A hash map for fast duplicate detection
/// - A single buffer to store all strings concatenated together
/// - A vector of end positions to track string boundaries
/// - A fast hasher for string hashing
///
/// # Type Parameters
///
/// * `I` - The ID type that must implement `PoolId`
#[derive(Debug, Default)]
pub struct StringPool<I: PoolId> {
    dedup: HashMap<u32, ()>,
    ends: Vec<u32>,
    buffer: String,
    hasher: RandomState,
    _marker: PhantomData<I>,
}

impl<I: PoolId> StringPool<I> {
    /// Creates a new empty string pool.
    pub fn new() -> Self {
        Self {
            dedup: HashMap::new(),
            ends: Vec::new(),
            buffer: String::new(),
            hasher: RandomState::new(),
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
    /// * `item` - The string to add to the pool
    ///
    /// # Returns
    ///
    /// The ID of the string in the pool
    pub fn add(&mut self, string: String) -> I {
        // Step 1: Hash the input string for fast lookup
        let hash = self.hasher.hash_one(&string);

        // Step 2: Try to find an existing entry with the same hash
        // We use the raw entry API to avoid double-hashing and provide custom equality
        let entry = self.dedup.raw_entry_mut().from_hash(hash, |id| {
            // Custom equality function: reconstruct the string from our buffer and compare it with the input string
            let span_str = Self::span_to_str(&self.buffer, &self.ends, *id);
            string == span_str
        });

        // Step 3: Handle the two cases - string exists or doesn't exist
        let (&mut id, &mut ()) = match entry {
            // Case 1: String already exists in the pool
            RawEntryMut::Occupied(occupied) => {
                // Return the existing ID without any modifications
                occupied.into_key_value()
            }
            // Case 2: String is new, add it to the pool
            RawEntryMut::Vacant(vacant) => {
                // Add the string to our contiguous buffer
                self.buffer.push_str(&string);
                let to = self.buffer.len();

                // Safety check: ensure we don't exceed u32 limits for position tracking
                if to > u32::MAX as usize {
                    panic!("Buffer size exceeded u32 limit");
                }

                // Generate a new ID (based on the number of strings we have)
                let id = self.ends.len();

                // Record where this string ends in the buffer
                self.ends.push(to as u32);

                // Insert the new entry into the hash map
                // We need to provide a hasher function for potential future rehashing
                vacant.insert_with_hasher(hash, id as u32, (), |id| {
                    // Rehashing function: reconstruct string from buffer and hash it
                    let span_str = Self::span_to_str(&self.buffer, &self.ends, *id);
                    self.hasher.hash_one(span_str)
                })
            }
        };

        // Step 4: Convert the internal u32 ID to the user's ID type
        I::from_u32(id)
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
    pub fn get(&self, id: &I) -> Option<String> {
        // Step 1: Convert the user's ID type to our internal u32 index
        let index = id.to_u32() as usize;

        // Step 2: Look up the end position for this string
        // If the index is out of bounds, this returns None and we short-circuit
        self.ends.get(index).map(|_| {
            // Step 3: Extract the string slice from our contiguous buffer using the helper function
            Self::span_to_str(&self.buffer, &self.ends, index as u32).to_string()
        })
    }

    /// Returns the number of unique strings in the pool.
    pub fn size(&self) -> usize {
        self.dedup.len()
    }

    /// Returns `true` if the pool contains no strings.
    pub fn empty(&self) -> bool {
        self.dedup.is_empty()
    }

    /// Helper function to extract a string slice from the buffer by ID.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it assumes:
    /// - The ID is valid (within bounds)
    /// - The buffer contains valid UTF-8 data
    /// - The position tracking is correct
    #[inline]
    fn span_to_str<'a>(buffer: &'a str, ends: &[u32], id: u32) -> &'a str {
        let to = ends[id as usize];
        let from = if id == 0 { 0 } else { ends[id as usize - 1] };
        unsafe { core::str::from_utf8_unchecked(&buffer.as_bytes()[from as usize..to as usize]) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test ID type for testing the Pool
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    struct TestId(u32);

    impl PoolId for TestId {
        fn from_u32(id: u32) -> Self {
            Self(id)
        }

        fn to_u32(&self) -> u32 {
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
        assert_eq!(pool.get(&id), Some("hello".to_string()));
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
        assert_eq!(pool.get(&id1), Some("hello".to_string()));
        assert_eq!(pool.get(&id2), Some("world".to_string()));
        assert_eq!(pool.get(&id3), Some("rust".to_string()));
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
        assert_eq!(pool.get(&id1), Some("hello".to_string()));
        assert_eq!(pool.get(&id2), Some("hello".to_string()));
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
        let invalid_id = TestId(999);
        assert_eq!(pool.get(&invalid_id), None);
    }

    #[test]
    fn pool_get_out_of_bounds_id_returns_none() {
        let mut pool: StringPool<TestId> = StringPool::new();

        // Add one item
        pool.add("hello".to_string());

        // Try to get with an ID that's out of bounds
        let out_of_bounds_id = TestId(100);
        assert_eq!(pool.get(&out_of_bounds_id), None);
    }

    #[test]
    fn pool_empty_string() {
        let mut pool: StringPool<TestId> = StringPool::new();

        let id = pool.add("".to_string());
        assert_eq!(pool.size(), 1);
        assert_eq!(pool.get(&id), Some("".to_string()));
    }

    #[test]
    fn pool_unicode_strings() {
        let mut pool: StringPool<TestId> = StringPool::new();

        let unicode_str = "Hello, 世界! 🦀";
        let id = pool.add(unicode_str.to_string());

        assert_eq!(pool.size(), 1);
        assert_eq!(pool.get(&id), Some(unicode_str.to_string()));
    }

    #[test]
    fn pool_long_string() {
        let mut pool: StringPool<TestId> = StringPool::new();

        let long_string = "a".repeat(1000);
        let id = pool.add(long_string.clone());

        assert_eq!(pool.size(), 1);
        assert_eq!(pool.get(&id), Some(long_string));
    }

    #[test]
    fn pool_strings_with_whitespace() {
        let mut pool: StringPool<TestId> = StringPool::new();

        let strings = vec!["  hello  ", "hello", " hello ", "\thello\n", "hello\r\n"];

        let mut ids = Vec::new();
        for s in &strings {
            ids.push(pool.add(s.to_string()));
        }

        // All strings are different due to whitespace
        assert_eq!(pool.size(), 5);

        // Verify each string is stored correctly
        for (i, s) in strings.iter().enumerate() {
            assert_eq!(pool.get(&ids[i]), Some(s.to_string()));
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

        assert_eq!(pool.get(&id1), Some("Hello".to_string()));
        assert_eq!(pool.get(&id2), Some("hello".to_string()));
        assert_eq!(pool.get(&id3), Some("HELLO".to_string()));
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
            assert_eq!(pool.get(id), Some(expected));
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
