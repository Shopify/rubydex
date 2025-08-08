//! This module contains stable ID representations that composed the `Index` graph

use blake3;

/// Creates a Blake3 hash from a string input and returns it as a 32-byte array.
fn create_hash(input: &str) -> [u8; 32] {
    let hash = blake3::hash(input.as_bytes());
    *hash.as_bytes()
}

// A UriId is the hashed version of a unique URI describing a resource. There cannot be two different resources
// described by the same exact URI
//
// Examples:
//  - file:///Users/someone/src/project/file.rb
//  - untitled:Untitled-1
//  - file:///C:/projects/something/file.rb
#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct UriId([u8; 32]);

impl UriId {
    #[must_use]
    pub fn new(id: &str) -> Self {
        Self(create_hash(id))
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
#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct DeclarationId([u8; 32]);

impl DeclarationId {
    #[must_use]
    pub fn new(id: &str) -> Self {
        Self(create_hash(id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_hash() {
        // Same input should produce same hash (deterministic)
        let hash1 = create_hash("test_input");
        let hash2 = create_hash("test_input");
        assert_eq!(hash1, hash2);

        // Different inputs should produce different hashes (unique)
        let hash3 = create_hash("different_input");
        assert_ne!(hash1, hash3);
    }
}
