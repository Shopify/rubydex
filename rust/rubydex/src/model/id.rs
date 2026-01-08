//! This module contains stable ID representations that compose the `Graph` global representation

use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::Deref,
};
use xxhash_rust::xxh3::xxh3_64;

/// A stable ID representation using i64.
///
/// We use i64 instead of u64 because `SQLite` doesn't support unsigned integers.
/// IDs are generated from `xxh3_64` hashes (u64) then cast to i64, preserving all bits.
/// Negative values are expected and normal - not a sign of memory corruption.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Id<T> {
    value: i64,
    _marker: PhantomData<T>,
}

impl<T> Id<T> {
    #[must_use]
    pub fn new(value: i64) -> Self {
        Self {
            value,
            _marker: PhantomData,
        }
    }
}

impl<T> Deref for Id<T> {
    type Target = i64;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> std::fmt::Display for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<T> Hash for Id<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_i64(self.value);
    }
}

impl<T> From<&str> for Id<T> {
    fn from(value: &str) -> Self {
        let hash = xxh3_64(value.as_bytes());
        Self::new(hash.cast_signed())
    }
}

impl<T> From<&String> for Id<T> {
    fn from(value: &String) -> Self {
        let hash = xxh3_64(value.as_bytes());
        Self::new(hash.cast_signed())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(PartialEq, Eq, Debug, Clone, Copy)]
    pub struct Marker;
    pub type TestId = Id<Marker>;

    #[test]
    fn test_create_hash() {
        // Same input should produce same hash (deterministic)
        let id1 = TestId::from("test_input");
        let id2 = TestId::from("test_input");
        assert_eq!(id1, id2);

        // Different inputs should produce different hashes (unique)
        let id3 = TestId::from("different_input");
        assert_ne!(id1, id3);
    }

    #[test]
    fn deref_unwraps_value() {
        let id = TestId::new(123);
        assert_eq!(*id, 123);
    }
}
