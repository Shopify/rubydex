use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
    num::NonZeroU64,
    ops::Deref,
};
use xxhash_rust::xxh3;

/// Maps a u64 hash to a `NonZeroU64` by replacing 0 with `u64::MAX`.
/// The probability of a 64-bit hash being exactly 0 is 2^-64 (~5.4e-20),
/// and remapping 0 → MAX just means those two inputs collide — the same
/// risk as any other hash collision, which the system already handles.
#[inline]
fn to_non_zero(value: u64) -> NonZeroU64 {
    NonZeroU64::new(value).unwrap_or(NonZeroU64::new(u64::MAX).unwrap())
}

/// A deterministic type-safe ID representation.
///
/// Uses `NonZeroU64` internally so that `Option<Id<T>>` is 8 bytes (same as `Id<T>`)
/// via niche optimization, instead of 16 bytes with a plain `u64`.
#[derive(Debug, Clone, Copy)]
pub struct Id<T> {
    value: NonZeroU64,
    _marker: PhantomData<T>,
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T> Eq for Id<T> {}

impl<T> Id<T> {
    #[must_use]
    pub fn new(value: u64) -> Self {
        Self {
            value: to_non_zero(value),
            _marker: PhantomData,
        }
    }
}

impl<T> Deref for Id<T> {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        // SAFETY: NonZeroU64 has the same layout as u64, and we only need the numeric value.
        // We return a reference to the inner u64 representation.
        unsafe { &*(std::ptr::from_ref(&self.value).cast::<u64>()) }
    }
}

impl<T> std::fmt::Display for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
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

    #[test]
    fn option_id_is_8_bytes() {
        assert_eq!(std::mem::size_of::<Option<TestId>>(), 8);
        assert_eq!(std::mem::size_of::<TestId>(), 8);
    }

    #[test]
    fn zero_hash_maps_to_nonzero() {
        let id = TestId::new(0);
        assert_eq!(*id, u64::MAX);
    }
}
