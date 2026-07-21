use std::{marker::PhantomData, num::NonZeroU64, ops::Deref};
use xxhash_rust::xxh3;

/// Creates an ID by hashing fixed-width integer components in little-endian order.
macro_rules! id_from_parts {
    ($id:ty; $($part:expr),+ $(,)?) => {
        <$id>::from([$(($part).to_le_bytes().as_slice()),+])
    };
}
pub(crate) use id_from_parts;

/// Maps a u64 hash to a `NonZeroU64` by replacing 0 with `u64::MAX`.
/// The probability of a 64-bit hash being exactly 0 is 2^-64 (~5.4e-20),
/// and remapping 0 → MAX just means those two inputs collide — the same
/// risk as any other hash collision, which the system already handles.
const MAX_NONZERO: NonZeroU64 = NonZeroU64::new(u64::MAX).unwrap();

#[inline]
fn to_non_zero(value: u64) -> NonZeroU64 {
    NonZeroU64::new(value).unwrap_or(MAX_NONZERO)
}

/// A deterministic type-safe ID representation.
///
/// Uses `NonZeroU64` internally so that `Option<Id<T>>` is 8 bytes (same as `Id<T>`)
/// via niche optimization, instead of 16 bytes with a plain `u64`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id<T> {
    value: NonZeroU64,
    _marker: PhantomData<T>,
}

impl<T> Id<T> {
    #[must_use]
    pub fn new(value: u64) -> Self {
        Self {
            value: to_non_zero(value),
            _marker: PhantomData,
        }
    }

    /// Returns the underlying `u64` value.
    #[must_use]
    pub fn get(&self) -> u64 {
        self.value.get()
    }
}

impl<T> Deref for Id<T> {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        // SAFETY: NonZeroU64 is #[repr(transparent)] over u64.
        // Deref requires returning &u64, but NonZeroU64::get() returns by value,
        // so the pointer cast is unavoidable here. Prefer Id::get() when a u64
        // value (not a reference) is sufficient.
        unsafe { &*std::ptr::from_ref(&self.value).cast::<u64>() }
    }
}

impl<T> std::fmt::Display for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
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

impl<T, const N: usize> From<[&[u8]; N]> for Id<T> {
    fn from(parts: [&[u8]; N]) -> Self {
        let mut hasher = xxh3::Xxh3Default::new();

        for part in parts {
            hasher.update(part);
        }

        Self::new(hasher.digest())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
    pub struct Marker;
    pub type TestId = Id<Marker>;

    #[test]
    fn from_byte_slices_matches_concatenated_string() {
        assert_eq!(
            TestId::from("foobar"),
            TestId::from([b"foo".as_slice(), b"bar".as_slice()]),
        );
    }

    #[test]
    fn from_parts_matches_mixed_width_byte_slices() {
        let id = 42_u64;
        let offset = 7_u32;
        let tag = 2_u8;

        assert_eq!(
            TestId::from([
                id.to_le_bytes().as_slice(),
                offset.to_le_bytes().as_slice(),
                tag.to_le_bytes().as_slice(),
            ]),
            id_from_parts!(TestId; id, offset, tag),
        );
    }

    #[test]
    fn from_byte_slices_is_deterministic() {
        let parts: [&[u8]; 2] = [&[1, 2, 3], &[4, 5]];
        assert_eq!(TestId::from(parts), TestId::from(parts));
    }

    #[test]
    fn from_byte_slices_distinguishes_inputs() {
        assert_ne!(TestId::from([b"foo".as_slice()]), TestId::from([b"bar".as_slice()]),);
    }

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
    fn get_returns_value() {
        let id = TestId::new(123);
        assert_eq!(id.get(), 123);
    }

    #[test]
    fn optional_id_is_still_8_bytes() {
        assert_eq!(std::mem::size_of::<Option<TestId>>(), 8);
        assert_eq!(std::mem::size_of::<TestId>(), 8);
    }

    #[test]
    fn zero_hash_maps_to_nonzero() {
        let id = TestId::new(0);
        assert_eq!(id.get(), u64::MAX);
    }
}
