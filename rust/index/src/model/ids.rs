//! This module contains stable ID representations that composed the `Index` graph

use std::{hash::DefaultHasher, hash::Hash, hash::Hasher};

// A UriId is the hashed version of a unique URI describing a resource. There cannot be two different resources
// described by the same exact URI
//
// Examples:
//  - file:///Users/someone/src/project/file.rb
//  - untitled:Untitled-1
//  - file:///C:/projects/something/file.rb
#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct UriId(u32);

impl UriId {
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn new(id: &str) -> Self {
        let mut hasher = DefaultHasher::new();
        id.hash(&mut hasher);
        let hash = hasher.finish() as u32;
        Self(hash)
    }
}

// A DeclarationId is the hashed version of the unique name representing a declaration. There cannot be two entities
// that shared the exact same name and so we can guarantee that IDs are both unique and stable, making it easy to load
// things from cache.
//
// Some example of unique names:
// - Foo
// - Foo::Bar
// - Foo::Bar#instance_method
#[derive(Hash, Eq, PartialEq)]
pub struct DeclarationId(u32);

impl DeclarationId {
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn new(id: &str) -> Self {
        let mut hasher = DefaultHasher::new();
        id.hash(&mut hasher);
        let hash = hasher.finish() as u32;
        Self(hash)
    }
}
