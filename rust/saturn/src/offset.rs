//! Offset handling for byte-based file positions.
//!
//! This module provides the [`Offset`] struct which represents a span of bytes
//! within a file. It can be used to track positions in source code and convert
//! between byte offsets and line/column positions.

pub const INVALID_OFFSET: u32 = u32::MAX;

/// Represents a byte offset range within a specific file.
///
/// An `Offset` tracks a contiguous span of bytes from `start` to `end` within a file. This is useful for
/// representing the location of tokens, AST nodes, or other text spans in source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Offset {
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
    /// * `start` - The starting byte position (inclusive)
    /// * `end` - The ending byte position (exclusive)
    #[must_use]
    pub const fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    /// Creates an invalid `Offset` with both start and end set to `INVALID_OFFSET`.
    ///
    /// This is useful for representing the absence of an offset, such as when a file is not found or if the
    /// error cannot be converted to an offset inside the file.
    #[must_use]
    pub const fn none() -> Self {
        Self {
            start: INVALID_OFFSET,
            end: INVALID_OFFSET,
        }
    }

    /// # Panics
    ///
    /// This function can panic if the Prism location offsets do not fit into a u32
    #[must_use]
    pub fn from_prism_location(location: &ruby_prism::Location) -> Self {
        Self::new(
            location
                .start_offset()
                .try_into()
                .expect("Expected usize `start` to fit in `u32`"),
            location
                .end_offset()
                .try_into()
                .expect("Expected usize `end` to fit in `u32`"),
        )
    }

    #[must_use]
    pub fn start(&self) -> u32 {
        self.start
    }

    #[must_use]
    pub fn end(&self) -> u32 {
        self.end
    }

    #[must_use]
    pub fn is_valid(&self) -> bool {
        self.start != INVALID_OFFSET && self.end != INVALID_OFFSET
    }
}
