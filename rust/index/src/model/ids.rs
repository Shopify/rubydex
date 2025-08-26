//! This module contains stable ID representations that composed the `Graph` global representation

use std::str::FromStr;

use blake3::Hash;

/// Creates a Blake3 hash from a string input and returns it as a 32-byte array.
fn create_hash(input: &str) -> Hash {
    blake3::hash(input.as_bytes())
}

// A UriId is the hashed version of a unique URI describing a resource. There cannot be two different resources
// described by the same exact URI
//
// Examples:
//  - file:///Users/someone/src/project/file.rb
//  - untitled:Untitled-1
//  - file:///C:/projects/something/file.rb
#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct UriId(Hash);

impl UriId {
    #[must_use]
    pub fn new(id: &str) -> Self {
        Self(create_hash(id))
    }

    /// # Panics
    ///
    /// This function will panic if invalid bytes were inserted directly into the database ID fields
    #[must_use]
    pub fn from_string(id: &str) -> Self {
        Self(
            Hash::from_str(id)
                .expect("Hash data contains invalid bytes. This means we introduced invalid data into the database"),
        )
    }
}

impl std::fmt::Display for UriId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// A NameId is the hashed version of the unique name representing a declaration. There cannot be two entities that
// shared the exact same name and so we can guarantee that IDs are both unique and stable, making it easy to load things
// from cache.
//
// Some example of unique names:
// - Foo
// - Foo::Bar
// - Foo::Bar#instance_method
#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct NameId(Hash);

impl NameId {
    #[must_use]
    pub fn new(id: &str) -> Self {
        Self(create_hash(id))
    }

    /// # Panics
    ///
    /// This function will panic if invalid bytes were inserted directly into the database ID fields
    #[must_use]
    pub fn from_string(id: &str) -> Self {
        Self(
            Hash::from_str(id)
                .expect("Hash data contains invalid bytes. This means we introduced invalid data into the database"),
        )
    }
}

impl std::fmt::Display for NameId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Hash, Eq, PartialEq, Debug, Copy, Clone)]
pub struct DefinitionId(Hash);

impl DefinitionId {
    #[must_use]
    pub fn new(uri_id: UriId, start: u32) -> Self {
        let id = format!("{uri_id}:{start}");
        Self(create_hash(&id))
    }

    /// # Panics
    ///
    /// This function will panic if invalid bytes were inserted directly into the database ID fields
    #[must_use]
    pub fn from_string(id: &str) -> Self {
        Self(
            Hash::from_str(id)
                .expect("Hash data contains invalid bytes. This means we introduced invalid data into the database"),
        )
    }
}

impl std::fmt::Display for DefinitionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
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
