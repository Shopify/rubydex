//! This module contains identity maps that use externally hashed IDs as keys. They are used to avoid hashing the same
//! value twice, simply using the given key directly

use std::{
    collections::{HashMap, HashSet},
    hash::{BuildHasher, Hasher},
};

#[derive(Default)]
pub struct IdentityHasher {
    hash: u64,
}

impl Hasher for IdentityHasher {
    fn write(&mut self, _bytes: &[u8]) {
        unreachable!("IdentityHasher only supports write_u64");
    }

    fn write_u32(&mut self, i: u32) {
        self.hash = u64::from(i);
    }

    fn write_u64(&mut self, i: u64) {
        self.hash = i;
    }

    fn finish(&self) -> u64 {
        self.hash
    }
}

#[derive(Default, Clone)]
pub struct IdentityHashBuilder;

impl BuildHasher for IdentityHashBuilder {
    type Hasher = IdentityHasher;

    fn build_hasher(&self) -> Self::Hasher {
        IdentityHasher::default()
    }
}

pub type IdentityHashMap<K, V> = HashMap<K, V, IdentityHashBuilder>;
pub type IdentityHashSet<T> = HashSet<T, IdentityHashBuilder>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identity_hasher_uses_value_as_is() {
        let builder = IdentityHashBuilder;
        let mut hasher = builder.build_hasher();

        hasher.write_u64(42);
        assert_eq!(hasher.finish(), 42);
    }
}
