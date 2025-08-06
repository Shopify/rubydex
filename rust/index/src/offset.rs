//! Offset handling for byte-based file positions.
//!
//! This module provides the [`Offset`] struct which represents a span of bytes
//! within a file. It can be used to track positions in source code and convert
//! between byte offsets and line/column positions.

/// Represents a byte offset range within a specific file.
///
/// An `Offset` tracks a contiguous span of bytes from `start_offset` to `end_offset`
/// within a file identified by `uri_id`. This is useful for representing the location
/// of tokens, AST nodes, or other text spans in source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Offset {
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
    pub const fn new(start_offset: u32, end_offset: u32) -> Self {
        Self {
            start_offset,
            end_offset,
        }
    }

    /// Creates an offset from a Prism location
    ///
    /// # Panics
    ///
    /// Will panic if the location cannot be contained in a u32
    #[must_use]
    pub fn from_prism_location(location: &ruby_prism::Location) -> Self {
        Self::new(
            location
                .start_offset()
                .try_into()
                .expect("Expected usize `start_offset` to fit in `u32`"),
            location
                .end_offset()
                .try_into()
                .expect("Expected usize `end_offset` to fit in `u32`"),
        )
    }
}
