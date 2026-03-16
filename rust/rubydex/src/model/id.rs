use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
    num::NonZeroU64,
    ops::Deref,
};
use xxhash_rust::xxh3;

/// Maps a u64 hash to NonZeroU64, sending 0 → u64::MAX.
/// This enables niche optimization: `Option<Id<T>>` is 8 bytes instead of 16.
#[inline]
fn to_nonzero(hash: u64) -> NonZeroU64 {
    NonZeroU64::new(hash).unwrap_or(NonZeroU64::new(u64::MAX).unwrap())
}

/// A deterministic type-safe ID representation.
/// Uses `NonZeroU64` internally so that `Option<Id<T>>` is 8 bytes (niche optimization).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Id<T> {
    value: NonZeroU64,
    _marker: PhantomData<T>,
}

impl<T> Id<T> {
    #[must_use]
    pub fn new(value: u64) -> Self {
        Self {
            value: to_nonzero(value),
            _marker: PhantomData,
        }
    }
}

impl<T> Deref for Id<T> {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        // SAFETY: NonZeroU64 has the same layout as u64
        // We need &u64, and NonZeroU64::get() returns u64 by value.
        // Use transmute of the reference to avoid changing the API.
        unsafe { std::mem::transmute(&self.value) }
    }
}

impl<T> std::fmt::Display for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<T> Hash for Id<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.value.get());
    }
}

impl<T> From<&str> for Id<T> {
    fn from(value: &str) -> Self {
        let hash = xxh3::xxh3_64(value.as_bytes());
        Self::new(hash)
    }
}

impl<T> From<&String> for Id<T> {
    fn from(value: &String) -> Self {
        let hash = xxh3::xxh3_64(value.as_bytes());
        Self::new(hash)
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
