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
        let loc = self.to_location(document).to_presentation();
        format!(
            "{}:{}-{}:{}",
            loc.start_line(),
            loc.start_col(),
            loc.end_line(),
            loc.end_col()
        )
    }

    /// Converts this offset to a 0-indexed [`Location`] with start and end line/column numbers.
    #[must_use]
    pub fn to_location(&self, document: &Document) -> Location {
        let line_index = document.line_index();
        let start = line_index.line_col(self.start().into());
        let end = line_index.line_col(self.end().into());
        Location {
            start_line: start.line,
            start_col: start.col,
            end_line: end.line,
            end_col: end.col,
        }
    }
}

/// A resolved location within a file, with start and end line/column positions.
/// Values are 0-indexed by default. Use [`to_presentation`](Location::to_presentation)
/// for 1-indexed values suitable for display.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    start_line: u32,
    start_col: u32,
    end_line: u32,
    end_col: u32,
}

impl Location {
    #[must_use]
    pub fn start_line(&self) -> u32 {
        self.start_line
    }

    #[must_use]
    pub fn start_col(&self) -> u32 {
        self.start_col
    }

    #[must_use]
    pub fn end_line(&self) -> u32 {
        self.end_line
    }

    #[must_use]
    pub fn end_col(&self) -> u32 {
        self.end_col
    }

    /// Returns a new `Location` with 1-indexed values for display purposes.
    #[must_use]
    pub fn to_presentation(&self) -> Self {
        Self {
            start_line: self.start_line + 1,
            start_col: self.start_col + 1,
            end_line: self.end_line + 1,
            end_col: self.end_col + 1,
        }
    }
}
