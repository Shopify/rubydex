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
///
/// # Examples
///
/// ```rust
/// use index::offset::Offset;
/// use index::pools::uri_pool::{UriId, UriPool};
///
/// let mut uri_pool = UriPool::new();
/// let uri_id = uri_pool.add("file://path/to/file.txt".to_string());
///
/// let offset = Offset::new(uri_id, 10, 20);
///
/// // Print the source covered by the offset
/// println!("{}", offset.source(&uri_pool));
///
/// // Create a location from the offset
/// let location = offset.to_location(&uri_pool);
/// println!("{}", location);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Offset {
    /// The ID of the uri this offset refers to (see `UriPool`)
    pub uri_id: UriId,
    /// The starting byte offset (inclusive)
    pub start_offset: u32,
    /// The ending byte offset (exclusive)
    pub end_offset: u32,
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
            start_offset,
            end_offset,
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

        format!("{}:{}-{}", uri, self.start_offset, self.end_offset)
    }
}
