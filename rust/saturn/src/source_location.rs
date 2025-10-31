#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Position {
    line: u32,
    column: u32,
}

impl Position {
    #[must_use]
    pub const fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }

    #[must_use]
    pub const fn line(&self) -> u32 {
        self.line
    }

    #[must_use]
    pub const fn column(&self) -> u32 {
        self.column
    }
}

pub trait SourceLocationConverter {
    /// Converts a byte offset to a Position (line and column).
    ///
    /// Note that the ``byte_offset`` is the number based on an UTF-8 encoded source code.
    ///
    /// Returns ``None`` if the byte offset is out of range.
    #[must_use]
    fn byte_offset_to_position(&self, byte_offset: u32) -> Option<Position> {
        let (line_num, line) = self.byte_offset_to_line_slice(byte_offset)?;
        Some(self.position_with_line_offset(line_num, byte_offset - line.start_offset, line.line_slice))
    }

    /// Converts a pair of byte offsets (start and end) to a pair of Positions.
    ///
    /// If both offsets are valid, returns ``Some((start_position, end_position))``.
    /// If either offset is invalid, returns ``None``.
    ///
    /// There are currently no optimization for this implementation.
    /// It simply translates each offset individually.
    #[must_use]
    fn offset_to_position(&self, offset: &crate::offset::Offset) -> Option<(Position, Position)> {
        let start = self.byte_offset_to_position(offset.start())?;
        let end = self.byte_offset_to_position(offset.end())?;
        Some((start, end))
    }

    /// Converts a Position (line and column) to a byte offset.
    ///
    /// Note that the ``byte_offset`` is the number based on an UTF-8 encoded source code.
    ///
    /// Returns ``None`` if the line number is out of range, or the column number is out of range of the line.
    ///
    #[must_use]
    fn byte_offset_from_position(&self, position: Position) -> Option<u32>;

    /// A helper function to get the line number and the source line struct.
    ///
    /// Returns ``None`` if the line number is out of range.
    #[must_use]
    fn byte_offset_to_line_slice(&self, byte_offset: u32) -> Option<(u32, SourceLine<'_>)>;

    /// A helper function to create a Position from a line number and a byte offset within the line.
    ///
    /// The ``line_slice`` is the string slice of the line, which can be used to calculate the column number.
    ///
    /// The ``line_offset`` and ``line_slice`` are assumed to be consistent.
    /// It panics if the ``line_offset`` is out of range of the ``line_slice``.
    #[must_use]
    fn position_with_line_offset(&self, line: u32, line_offset: u32, line_slice: &str) -> Position;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SourceLine<'a> {
    /// The byte offset of the beginning of the line in the entire text
    start_offset: u32,

    /// The slice of the line, with or without the '\n' at the end
    line_slice: &'a str,

    /// The length of the line in bytes without new-line char
    line_len: u32,
}

/// A helper struct to hold the source text and precomputed line bytes offsets
#[derive(Debug)]
pub struct SourceLines<'a> {
    /// The byte size of the entire text
    text_len: u32,

    /// A vector of (byte offset of the line start, line slice, line length in bytes without new-line char).
    ///
    /// The lines end with '\n' except for the last line.
    /// If the content ends with a newline, there will be an empty line at the end.
    lines: Vec<SourceLine<'a>>,
}

impl<'a> SourceLines<'a> {
    #[must_use]
    pub fn new(len: u32, lines: Vec<SourceLine<'a>>) -> Self {
        Self { text_len: len, lines }
    }

    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn from_text(text: &'a str) -> Self {
        let mut line_start = 0usize;
        let mut lines: Vec<SourceLine<'a>> = Vec::new();

        for (byte_pos, ch) in text.char_indices() {
            if ch == '\n' {
                let line_slice = &text[line_start..=byte_pos];
                lines.push(SourceLine {
                    start_offset: line_start as u32,
                    line_slice,
                    line_len: (byte_pos - line_start) as u32,
                });

                // Start a new line, with additional byte for '\n'
                line_start = byte_pos + 1;
            }
        }

        let line_slice = &text[line_start..];
        lines.push(SourceLine {
            start_offset: line_start as u32,
            line_slice,
            line_len: line_slice.len() as u32,
        });

        SourceLines::new(text.len() as u32, lines)
    }

    /// Returns the possible line number of the given byte offset.
    ///
    /// Returns ``None`` if the byte offset is out of range.
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn get_line_number(&self, byte_offset: u32) -> Option<u32> {
        if byte_offset > self.text_len {
            return None;
        }

        if byte_offset == 0 {
            return Some(0);
        }

        if byte_offset == self.text_len {
            return Some(self.lines.len() as u32 - 1);
        }

        let line_index = match self.lines.binary_search_by_key(&byte_offset, |line| line.start_offset) {
            Ok(line_index) => line_index,
            Err(line_index) => line_index.saturating_sub(1),
        };

        Some(line_index as u32)
    }

    fn get_line_slice_from_byte_offset(&self, byte_offset: u32) -> Option<(u32, SourceLine<'a>)> {
        let line = self.get_line_number(byte_offset)?;
        let line_data = self.lines.get(line as usize)?;
        Some((line, *line_data))
    }
}

/// A source location converter for UTF-8 encoding.
///
/// The column number is the UTF-8 code units with the line. (This is equivalent to the byte offset.)
pub struct UTF8SourceLocationConverter<'a> {
    source: SourceLines<'a>,
}

impl<'a> UTF8SourceLocationConverter<'a> {
    #[must_use]
    pub fn new(text: &'a str) -> Self {
        let source = SourceLines::from_text(text);
        Self { source }
    }
}

impl SourceLocationConverter for UTF8SourceLocationConverter<'_> {
    fn byte_offset_to_line_slice(&self, byte_offset: u32) -> Option<(u32, SourceLine<'_>)> {
        self.source.get_line_slice_from_byte_offset(byte_offset)
    }

    fn position_with_line_offset(&self, line: u32, line_offset: u32, _line_slice: &str) -> Position {
        Position {
            line,
            column: line_offset,
        }
    }

    fn byte_offset_from_position(&self, position: Position) -> Option<u32> {
        let line = self.source.lines.get(position.line as usize)?;

        if position.column > line.line_len {
            return None;
        }

        let offset = line.start_offset + position.column;

        if offset <= self.source.text_len {
            Some(offset)
        } else {
            None
        }
    }
}

/// A source location converter for UTF-16 encoding.
///
/// The column number is the UTF-16 code units with the line.
/// 1 for most of the unicode characters.
/// 2 for characters outside the BMP (Basic Multilingual Plane) that are represented as surrogate pairs.
pub struct UTF16SourceLocationConverter<'a> {
    source: SourceLines<'a>,
}

impl<'a> UTF16SourceLocationConverter<'a> {
    #[must_use]
    pub fn new(text: &'a str) -> Self {
        let source = SourceLines::from_text(text);
        Self { source }
    }
}

impl SourceLocationConverter for UTF16SourceLocationConverter<'_> {
    fn byte_offset_to_line_slice(&self, byte_offset: u32) -> Option<(u32, SourceLine<'_>)> {
        self.source.get_line_slice_from_byte_offset(byte_offset)
    }

    #[allow(clippy::cast_possible_truncation)]
    fn position_with_line_offset(&self, line: u32, line_offset: u32, line_slice: &str) -> Position {
        let mut column = 0u32;

        for (byte_pos, ch) in line_slice.char_indices() {
            if byte_pos as u32 == line_offset {
                break;
            }
            assert!(
                (byte_pos as u32) < line_offset,
                "The line_offset doesn't look like it's on a boundary between characters: {line_offset}, {line_slice}"
            );

            column += ch.len_utf16() as u32;
        }

        Position { line, column }
    }

    #[allow(clippy::cast_possible_truncation)]
    fn byte_offset_from_position(&self, position: Position) -> Option<u32> {
        let source_line = self.source.lines.get(position.line as usize)?;

        let mut current_column = 0u32;

        if position.column == 0 {
            return Some(source_line.start_offset);
        }

        let char_indices = source_line
            .line_slice
            .char_indices()
            .take(source_line.line_len as usize);

        for (byte_pos, ch) in char_indices {
            let code_units = ch.len_utf16() as u32;
            current_column += code_units;

            if current_column == position.column {
                return Some(source_line.start_offset + byte_pos as u32 + ch.len_utf8() as u32);
            }
        }

        None
    }
}

/// A source location converter for UTF-32 position encoding.
///
/// The column number is the UTF-32 code units with the line.
pub struct UTF32SourceLocationConverter<'a> {
    source: SourceLines<'a>,
}

impl<'a> UTF32SourceLocationConverter<'a> {
    #[must_use]
    pub fn new(text: &'a str) -> Self {
        Self {
            source: SourceLines::from_text(text),
        }
    }
}

impl SourceLocationConverter for UTF32SourceLocationConverter<'_> {
    fn byte_offset_to_line_slice(&self, byte_offset: u32) -> Option<(u32, SourceLine<'_>)> {
        self.source.get_line_slice_from_byte_offset(byte_offset)
    }

    #[allow(clippy::cast_possible_truncation)]
    fn position_with_line_offset(&self, line: u32, line_offset: u32, line_slice: &str) -> Position {
        let mut column = 0u32;

        for (byte_pos, _ch) in line_slice.char_indices() {
            if byte_pos as u32 == line_offset {
                break;
            }
            assert!(
                (byte_pos as u32) < line_offset,
                "The line_offset doesn't look like it's on a boundary between characters: {line_offset}, {line_slice}"
            );

            column += 1;
        }

        Position { line, column }
    }

    #[allow(clippy::cast_possible_truncation)]
    fn byte_offset_from_position(&self, position: Position) -> Option<u32> {
        let source_line = self.source.lines.get(position.line as usize)?;

        if position.column == 0 {
            return Some(source_line.start_offset);
        }

        match source_line.line_slice.char_indices().nth(position.column as usize) {
            Some((byte_pos, _ch)) => {
                let offset = source_line.start_offset + byte_pos as u32;
                Some(offset)
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_lines_1() {
        let text = String::from("Hello\n世界\n");
        let source = SourceLines::from_text(&text);

        assert_eq!(source.text_len, 13);
        assert_eq!(
            source.lines,
            vec![
                SourceLine {
                    start_offset: 0,
                    line_slice: "Hello\n",
                    line_len: 5
                },
                SourceLine {
                    start_offset: 6,
                    line_slice: "世界\n",
                    line_len: 6
                },
                SourceLine {
                    start_offset: 13,
                    line_slice: "",
                    line_len: 0
                },
            ]
        );
    }

    #[test]
    fn test_source_lines_2() {
        let text = String::from("Hello\n世界");
        let source = SourceLines::from_text(&text);

        assert_eq!(source.text_len, 12);
        assert_eq!(
            source.lines,
            vec![
                SourceLine {
                    start_offset: 0,
                    line_slice: "Hello\n",
                    line_len: 5
                },
                SourceLine {
                    start_offset: 6,
                    line_slice: "世界",
                    line_len: 6
                },
            ]
        );
    }

    #[test]
    fn test_source_lines_3() {
        let text = String::new();
        let source = SourceLines::from_text(&text);

        assert_eq!(source.text_len, 0);
        assert_eq!(
            source.lines,
            vec![SourceLine {
                start_offset: 0,
                line_slice: "",
                line_len: 0
            }]
        );
    }

    /// The characters are 1 byte each in UTF-8, 1 code-unit in UTF-16, and 1 code-unit in UTF-32
    static ASCII_STRING: &str = "Hello\nWorld\n";
    /// The kanji characters are 3 bytes each in UTF-8, 1 code-unit in UTF-16, and 1 code-unit in UTF-32
    static MULTIBYTE_STRING: &str = "こんにちは\n世界\n";
    /// The emoji characters are 4 bytes each in UTF-8, 2 code-units in UTF-16, and 1 code-unit in UTF-32
    static SURROGATE_PAIR_STRING: &str = "😀\n🍔\n";

    fn assert_location_converter(
        converter: &dyn SourceLocationConverter,
        successful_pairs: &[(u32, Position)],
        fail_offsets: &[u32],
        fail_positions: &[Position],
    ) {
        for (byte_offset, expected_position) in successful_pairs {
            let position = converter.byte_offset_to_position(*byte_offset);
            assert_eq!(position, Some(*expected_position));

            let back_offset = converter.byte_offset_from_position(*expected_position);
            assert_eq!(back_offset, Some(*byte_offset));
        }

        for byte_offset in fail_offsets {
            let position = converter.byte_offset_to_position(*byte_offset);
            assert_eq!(position, None);
        }

        for position in fail_positions {
            let back_offset = converter.byte_offset_from_position(*position);
            assert_eq!(back_offset, None);
        }
    }

    #[test]
    fn test_utf8_converter() {
        assert_location_converter(
            &UTF8SourceLocationConverter::new(ASCII_STRING),
            &[
                (0, Position { line: 0, column: 0 }),
                (1, Position { line: 0, column: 1 }),
                (5, Position { line: 0, column: 5 }),
                (6, Position { line: 1, column: 0 }),
                (11, Position { line: 1, column: 5 }),
                (12, Position { line: 2, column: 0 }),
            ],
            &[13],
            &[
                Position { line: 2, column: 1 },
                Position { line: 3, column: 0 }, // The input has only 3 lines
                Position { line: 0, column: 6 }, // The first line has only 5 characters
            ],
        );

        assert_location_converter(
            &UTF8SourceLocationConverter::new(MULTIBYTE_STRING),
            &[
                (0, Position { line: 0, column: 0 }),
                (3, Position { line: 0, column: 3 }),
                (15, Position { line: 0, column: 15 }),
                (16, Position { line: 1, column: 0 }),
                (22, Position { line: 1, column: 6 }),
                (23, Position { line: 2, column: 0 }),
            ],
            &[24],
            &[Position { line: 2, column: 1 }],
        );

        assert_location_converter(
            &UTF8SourceLocationConverter::new(SURROGATE_PAIR_STRING),
            &[
                (0, Position { line: 0, column: 0 }),
                (4, Position { line: 0, column: 4 }),
                (5, Position { line: 1, column: 0 }),
                (9, Position { line: 1, column: 4 }),
                (10, Position { line: 2, column: 0 }),
            ],
            &[11],
            &[Position { line: 2, column: 1 }],
        );
    }

    #[test]
    fn test_utf16_converter() {
        assert_location_converter(
            &UTF16SourceLocationConverter::new(ASCII_STRING),
            &[
                (0, Position { line: 0, column: 0 }),
                (1, Position { line: 0, column: 1 }),
                (5, Position { line: 0, column: 5 }),
                (6, Position { line: 1, column: 0 }),
                (11, Position { line: 1, column: 5 }),
                (12, Position { line: 2, column: 0 }),
            ],
            &[13],
            &[
                Position { line: 2, column: 1 },
                Position { line: 3, column: 0 }, // The input has only 3 lines
                Position { line: 0, column: 6 }, // The first line has only 5 characters
            ],
        );

        assert_location_converter(
            &UTF16SourceLocationConverter::new(MULTIBYTE_STRING),
            &[
                (0, Position { line: 0, column: 0 }),
                (3, Position { line: 0, column: 1 }),
                (15, Position { line: 0, column: 5 }),
                (16, Position { line: 1, column: 0 }),
                (22, Position { line: 1, column: 2 }),
                (23, Position { line: 2, column: 0 }),
            ],
            &[24],
            &[Position { line: 2, column: 1 }],
        );

        assert_location_converter(
            &UTF16SourceLocationConverter::new(SURROGATE_PAIR_STRING),
            &[
                (0, Position { line: 0, column: 0 }),
                (4, Position { line: 0, column: 2 }),
                (5, Position { line: 1, column: 0 }),
                (9, Position { line: 1, column: 2 }),
                (10, Position { line: 2, column: 0 }),
            ],
            &[11],
            &[Position { line: 2, column: 1 }],
        );
    }

    #[test]
    fn test_utf32_converter() {
        assert_location_converter(
            &UTF32SourceLocationConverter::new(ASCII_STRING),
            &[
                (0, Position { line: 0, column: 0 }),
                (1, Position { line: 0, column: 1 }),
                (5, Position { line: 0, column: 5 }),
                (6, Position { line: 1, column: 0 }),
                (11, Position { line: 1, column: 5 }),
                (12, Position { line: 2, column: 0 }),
            ],
            &[13],
            &[
                Position { line: 2, column: 1 },
                Position { line: 3, column: 0 }, // The input has only 3 lines
                Position { line: 0, column: 6 }, // The first line has only 5 characters
            ],
        );

        assert_location_converter(
            &UTF32SourceLocationConverter::new(MULTIBYTE_STRING),
            &[
                (0, Position { line: 0, column: 0 }),
                (3, Position { line: 0, column: 1 }),
                (15, Position { line: 0, column: 5 }),
                (16, Position { line: 1, column: 0 }),
                (22, Position { line: 1, column: 2 }),
                (23, Position { line: 2, column: 0 }),
            ],
            &[24],
            &[Position { line: 2, column: 1 }],
        );

        assert_location_converter(
            &UTF32SourceLocationConverter::new(SURROGATE_PAIR_STRING),
            &[
                (0, Position { line: 0, column: 0 }),
                (4, Position { line: 0, column: 1 }),
                (5, Position { line: 1, column: 0 }),
                (9, Position { line: 1, column: 1 }),
                (10, Position { line: 2, column: 0 }),
            ],
            &[11],
            &[Position { line: 2, column: 1 }],
        );
    }
}
