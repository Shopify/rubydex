//! Offset handling for byte-based file positions.
//!
//! This module provides the [`Offset`] struct which represents a span of bytes
//! within a file. It can be used to track positions in source code and convert
//! between byte offsets and line/column positions.

use crate::model::document::Document;

/// Represents a byte offset range within a specific file.
///
/// An `Offset` tracks a contiguous span of bytes from `start` to `end` within a file. This is useful for
/// representing the location of tokens, AST nodes, or other text spans in source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
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

    /// # Panics
    ///
    /// This function can panic if the RBS location offsets don't fit into u32
    #[must_use]
    pub fn from_rbs_location(location: &ruby_rbs::node::RBSLocationRange) -> Self {
        Self::new(
            location
                .start()
                .try_into()
                .expect("RBS location start offset should fit into u32"),
            location
                .end()
                .try_into()
                .expect("RBS location end offset should fit into u32"),
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

    /// Converts an offset to a display range like `1:1-1:5`
    #[must_use]
    pub fn to_display_range(&self, document: &Document) -> String {
        let line_index = document.line_index();
        let start = line_index.line_col(self.start().into());
        let end = line_index.line_col(self.end().into());
        format!("{}:{}-{}:{}", start.line + 1, start.col + 1, end.line + 1, end.col + 1)
    }
}
