//! Uri management with efficient string interning.
//!
//! This module provides a specialized [`StringPool`] for managing uri strings efficiently.
//! It uses string interning to ensure that identical uri strings are stored only once
//! in memory, with each unique uri identified by a compact [`UriId`].
//!
//! # Examples
//!
//! ```rust
//! use index::pools::uri_pool::{UriId, UriPool};
//!
//! let mut pool = UriPool::new();
//!
//! // Add uri strings to the pool
//! let uri1 = pool.add("file:///path/to/file1.txt".to_string());
//! let uri2 = pool.add("file:///path/to/file2.txt".to_string());
//! let uri3 = pool.add("file:///path/to/file1.txt".to_string()); // Same uri
//!
//! // String interning: same uris get the same ID
//! assert_ne!(uri1, uri2);
//! assert_eq!(uri1, uri3);
//!
//! // Retrieve uris by ID
//! assert_eq!(pool.get(uri1), Some("file:///path/to/file1.txt".to_string()));
//! assert_eq!(pool.get(uri2), Some("file:///path/to/file2.txt".to_string()));
//! ```

use crate::pools::string_pool::{PoolId, StringPool, SymbolId};

/// A unique identifier for a uri in a [`UriPool`].
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct UriId(SymbolId);

impl PoolId for UriId {
    fn from_symbol(symbol: SymbolId) -> Self {
        Self(symbol)
    }

    fn to_symbol(&self) -> SymbolId {
        self.0
    }
}

/// A specialized string pool for efficient uri storage and retrieval using [`UriId`].
pub type UriPool = StringPool<UriId>;
