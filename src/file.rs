use std::fmt::Display;

use crate::location::Location;
use crate::offset::Offset;
use crate::pool::{Pool, PoolId};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FileId(u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct File {
    pub name_id: PoolId<FileId>,
}

impl File {
    pub fn new(name_id: PoolId<FileId>) -> Self {
        Self { name_id }
    }

    pub fn offset_to_location(&self, pool: &Pool<FileId, String>, offset: Offset) -> Location {
        let file = pool.get(&self.name_id).unwrap();
        let content = std::fs::read_to_string(file).unwrap();

        // Convert byte offsets to line and column numbers
        let (start_line, start_column) = Self::offset_to_line_col(&content, offset.start_offset);
        let (end_line, end_column) = Self::offset_to_line_col(&content, offset.end_offset);

        Location {
            file: file.clone(),
            start_line,
            end_line,
            start_column,
            end_column,
        }
    }

    fn offset_to_line_col(content: &str, offset: u32) -> (u32, u32) {
        let mut line = 1;
        let mut column = 1;
        let mut current_offset = 0;

        for ch in content.chars() {
            if current_offset >= offset {
                break;
            }

            if ch == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }

            current_offset += ch.len_utf8() as u32;
        }

        (line, column)
    }
}

impl Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.name_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    fn create_temp_file(content: &str) -> (NamedTempFile, Pool<FileId, String>, File) {
        let mut temp_file = NamedTempFile::new().unwrap();
        temp_file.write_all(content.as_bytes()).unwrap();
        temp_file.flush().unwrap();

        let mut pool = Pool::new();
        let file_path = temp_file.path().to_string_lossy().to_string();
        let name_id = pool.add(file_path);
        let file = File { name_id };

        (temp_file, pool, file)
    }

    #[test]
    fn test_basic_offset_to_location() {
        let content = "Hello\nWorld";

        let (_temp_file, pool, file) = create_temp_file(content);

        // Test start of file
        let offset = Offset { start_offset: 0, end_offset: 5 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 6);
        assert_eq!(location.source(), "Hello");

        // Test across newline
        let offset = Offset { start_offset: 4, end_offset: 7 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 5);
        assert_eq!(location.end_line, 2);
        assert_eq!(location.end_column, 2);
        assert_eq!(location.source(), "o\nW");
    }

    #[test]
    fn test_single_line_file() {
        let content = "Hello World";
        let (_temp_file, pool, file) = create_temp_file(content);

        let offset = Offset { start_offset: 0, end_offset: 11 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 12);
        assert_eq!(location.source(), "Hello World");
    }

    #[test]
    fn test_empty_file() {
        let content = "";
        let (_temp_file, pool, file) = create_temp_file(content);

        let offset = Offset { start_offset: 0, end_offset: 0 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 1);
        assert_eq!(location.source(), "");
    }

    #[test]
    fn test_utf8_characters() {
        let content = "Hello 🌍 World\nUTF-8 测试\n";
        let (_temp_file, pool, file) = create_temp_file(content);

        // Test position before emoji
        let offset = Offset { start_offset: 0, end_offset: 6 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 7);
        assert_eq!(location.source(), "Hello ");

        // Test position at emoji (🌍 is 4 bytes but 1 character)
        let offset = Offset { start_offset: 6, end_offset: 10 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 7);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 8);
        assert_eq!(location.source(), "🌍");

        // Test position after emoji (🌍 is 4 bytes but 1 character)
        let offset = Offset { start_offset: 10, end_offset: 16 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 8);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 14);
        assert_eq!(location.source(), " World");

        // Test Chinese characters (测试 are 3 bytes each but 1 character each)
        let offset = Offset { start_offset: 17, end_offset: 27 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 2);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_line, 2);
        assert_eq!(location.end_column, 9);
        assert_eq!(location.source(), "UTF-8 测试");
    }

    #[test]
    fn test_multiple_newlines() {
        let content = "Line 1\n\nLine 3\n\n\nLine 6";
        let (_temp_file, pool, file) = create_temp_file(content);

        // Test empty line
        let offset = Offset { start_offset: 7, end_offset: 8 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 2);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_line, 3);
        assert_eq!(location.end_column, 1);
        assert_eq!(location.source(), "\n");

        // Test after multiple empty lines
        let offset = Offset { start_offset: 14, end_offset: 17 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 3);
        assert_eq!(location.start_column, 7);
        assert_eq!(location.end_line, 6);
        assert_eq!(location.end_column, 1);
        assert_eq!(location.source(), "\n\n\n");
    }

    #[test]
    fn test_newline_only_file() {
        let content = "\n\n\n";
        let (_temp_file, pool, file) = create_temp_file(content);

        let offset = Offset { start_offset: 0, end_offset: 3 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_line, 4);
        assert_eq!(location.end_column, 1);
    }

    #[test]
    fn test_zero_width_offset() {
        let content = "Hello\nWorld";
        let (_temp_file, pool, file) = create_temp_file(content);

        // Test zero-width offset at beginning of line
        let offset = Offset { start_offset: 6, end_offset: 6 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 2);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_line, 2);
        assert_eq!(location.end_column, 1);
        assert_eq!(location.source(), "");

        // Test zero-width offset in middle of line
        let offset = Offset { start_offset: 3, end_offset: 3 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 4);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_column, 4);
        assert_eq!(location.source(), "");
    }

    #[test]
    fn test_end_of_file_offset() {
        let content = "Hello\nWorld";
        let (_temp_file, pool, file) = create_temp_file(content);

        // Test offset at end of file
        let offset = Offset { start_offset: 11, end_offset: 11 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 2);
        assert_eq!(location.start_column, 6);
        assert_eq!(location.end_line, 2);
        assert_eq!(location.end_column, 6);
        assert_eq!(location.source(), "");
    }

    #[test]
    fn test_mixed_unicode_with_newlines() {
        let content = "🚀 Rust\n是很棒的\n语言! 👍";
        let (_temp_file, pool, file) = create_temp_file(content);

        // Test across multiple lines with mixed unicode
        let offset = Offset { start_offset: 0, end_offset: 27 };
        let location = file.offset_to_location(&pool, offset);
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_column, 1);
        assert_eq!(location.end_line, 3);
        assert_eq!(location.end_column, 3);
        assert_eq!(location.source(), "🚀 Rust\n是很棒的\n语言");
    }
}