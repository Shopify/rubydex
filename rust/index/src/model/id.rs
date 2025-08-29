//! This module contains stable ID representations that compose the `Graph` global representation

use serde::{Deserialize, Serialize};
use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::Deref,
};
use xxhash_rust::xxh3::xxh3_64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Id<T> {
    value: u64,
    _marker: PhantomData<T>,
}

impl<T> Id<T> {
    #[must_use]
    pub fn new(value: u64) -> Self {
        Self {
            value,
            _marker: PhantomData,
        }
    }
}

impl<T> Deref for Id<T> {
    type Target = u64;

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
        state.write_u64(self.value);
    }
}

impl<T> From<&str> for Id<T> {
    fn from(value: &str) -> Self {
        Self::new(xxh3_64(value.as_bytes()))
    }
}

impl<T> From<&String> for Id<T> {
    fn from(value: &String) -> Self {
        Self::new(xxh3_64(value.as_bytes()))
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
