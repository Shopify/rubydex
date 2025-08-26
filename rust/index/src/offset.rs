//! Offset handling for byte-based file positions.
//!
//! This module provides the [`Offset`] struct which represents a span of bytes
//! within a file. It can be used to track positions in source code and convert
//! between byte offsets and line/column positions.

use serde::{Deserialize, Serialize};

/// Represents a byte offset range within a specific file.
///
/// An `Offset` tracks a contiguous span of bytes from `start` to `end` within a file. This is useful for
/// representing the location of tokens, AST nodes, or other text spans in source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
}
