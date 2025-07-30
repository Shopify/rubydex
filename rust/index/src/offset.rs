//! Offset handling for byte-based file positions.
//!
//! This module provides the [`Offset`] struct which represents a span of bytes
//! within a file. It can be used to track positions in source code and convert
//! between byte offsets and line/column positions.

use crate::pools::uri_pool::{UriId, UriPool};

/// Represents a byte offset range within a specific file.
///
/// An `Offset` tracks a contiguous span of bytes from `start_offset` to `end_offset`
/// within a file identified by `uri_id`. This is useful for representing the location
/// of tokens, AST nodes, or other text spans in source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Offset {
    /// The ID of the uri this offset refers to (see `UriPool`)
    uri_id: UriId,
    /// The starting byte offset (inclusive)
    start: u32,
    /// The ending byte offset (exclusive)
    end: u32,
}

impl Offset {
    /// Creates a new `Offset` with the specified file ID and byte range.
    ///
    /// # Arguments
    ///
    /// * `uri_id` - The ID of the uri this offset refers to (see `UriPool`)
    /// * `start_offset` - The starting byte position (inclusive)
    /// * `end_offset` - The ending byte position (exclusive)
    #[must_use]
    pub const fn new(uri_id: UriId, start_offset: u32, end_offset: u32) -> Self {
        Self {
            uri_id,
            start: start_offset,
            end: end_offset,
        }
    }

    /// Returns a string representation of this offset in the format "URI:start-end".
    ///
    /// # Arguments
    ///
    /// * `uri_pool` - The uri pool to resolve the uri from the uri ID
    ///
    /// # Returns
    ///
    /// A string in the format "URI:start_offset-end_offset"
    ///
    /// # Panics
    ///
    /// Panics if the uri ID is not found in the uri pool.
    #[must_use]
    pub fn to_string(&self, uri_pool: &UriPool) -> String {
        let uri = uri_pool.get(self.uri_id).unwrap();

        format!("{}:{}-{}", uri, self.start, self.end)
    }
}
