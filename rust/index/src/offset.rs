//! Offset handling for byte-based file positions.
//!
//! This module provides the [`Offset`] struct which represents a span of bytes
//! within a file. It can be used to track positions in source code and convert
//! between byte offsets and line/column positions.

use crate::model::ids::UriId;

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
    start_offset: u32,
    /// The ending byte offset (exclusive)
    end_offset: u32,
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

    #[must_use]
    pub fn uri_id(&self) -> UriId {
        self.uri_id
    }
}
