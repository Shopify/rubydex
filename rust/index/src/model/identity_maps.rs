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
    fn write(&mut self, _bytes: &[u8]) {}

    fn write_u64(&mut self, i: u64) {
        self.hash = i;
    }

    fn finish(&self) -> u64 {
        self.hash
    }
}

#[derive(Default)]
pub struct IdentityHashBuilder;

impl BuildHasher for IdentityHashBuilder {
    type Hasher = IdentityHasher;

    fn build_hasher(&self) -> Self::Hasher {
        IdentityHasher::default()
    }
}

pub type IdentityHashMap<K, V> = HashMap<K, V, IdentityHashBuilder>;
pub type IdentityHashSet<K> = HashSet<K, IdentityHashBuilder>;
