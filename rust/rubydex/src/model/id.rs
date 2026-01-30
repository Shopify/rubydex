use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::Deref,
};
use xxhash_rust::xxh32;

const TAG_BITS: u32 = 4;
const TAG_MASK: u32 = (1 << TAG_BITS) - 1;

pub trait Taggable {
    type Kind: Into<u8> + From<u8>;
}

impl<T: Taggable> Id<T> {
    pub fn tag_kind(&mut self, kind: T::Kind) {
        self.value = (self.value & !TAG_MASK) | u32::from(kind.into());
    }

    #[must_use]
    pub fn kind(&self) -> T::Kind {
        ((self.value & TAG_MASK) as u8).into()
    }
}

/// A deterministic type-safe ID representation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Id<T> {
    value: u32,
    _marker: PhantomData<T>,
}

impl<T> Id<T> {
    #[must_use]
    pub fn new(value: u32) -> Self {
        Self {
            value,
            _marker: PhantomData,
        }
    }
}

impl<T> Deref for Id<T> {
    type Target = u32;

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
        state.write_u32(self.value);
    }
}

impl<T> From<&str> for Id<T> {
    fn from(value: &str) -> Self {
        let hash = xxh32::xxh32(value.as_bytes(), 0);
        Self::new(hash)
    }
}

impl<T> From<&String> for Id<T> {
    fn from(value: &String) -> Self {
        let hash = xxh32::xxh32(value.as_bytes(), 0);
        Self::new(hash)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(PartialEq, Eq, Debug, Clone, Copy)]
    pub struct Marker;
    impl Taggable for Marker {
        type Kind = u8;
    }
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

    #[test]
    fn tagging_preserves_higher_bits() {
        let mut id = TestId::new(0x1234_1234);
        id.tag_kind(0b0000_1111);
        assert_eq!(*id, 0x1234_123F);
        assert_eq!(id.kind(), 0b0000_1111);
    }
}
