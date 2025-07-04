use std::fmt::Display;

#[derive(Debug)]
pub struct Location {
    pub file: String,
    pub start_line: u32,
    pub end_line: u32,
    pub start_column: u32,
    pub end_column: u32,
}

impl Location {
    pub fn new(file: String, start_line: u32, start_column: u32, end_line: u32, end_column: u32) -> Self {
        Location {
            file,
            start_line,
            end_line,
            start_column,
            end_column,
        }
    }

    pub fn source(&self) -> String {
        let content = std::fs::read_to_string(self.file.clone()).unwrap();
        let lines: Vec<&str> = content.lines().collect();

        // Handle empty file
        if lines.is_empty() {
            return String::new();
        }

        // Convert to 0-indexed
        let start_line_idx = (self.start_line - 1) as usize;
        let end_line_idx = (self.end_line - 1) as usize;
        let start_col_idx = (self.start_column - 1) as usize;
        let end_col_idx = (self.end_column - 1) as usize;

        // Handle case where line indices are out of bounds
        if start_line_idx >= lines.len() {
            return String::new();
        }

        let actual_end_line_idx = std::cmp::min(end_line_idx, lines.len() - 1);

        if start_line_idx == actual_end_line_idx {
            // Single line case
            let line = lines[start_line_idx];
            let chars: Vec<char> = line.chars().collect();

            let actual_start_col = std::cmp::min(start_col_idx, chars.len());
            let actual_end_col = std::cmp::min(end_col_idx, chars.len());

            if actual_start_col >= actual_end_col {
                return String::new();
            }

            chars[actual_start_col..actual_end_col].iter().collect()
        } else {
            // Multi-line case
            let mut result = String::new();

            for (i, line_idx) in (start_line_idx..=actual_end_line_idx).enumerate() {
                let line = lines[line_idx];
                let chars: Vec<char> = line.chars().collect();

                if i == 0 {
                    // First line - from start_column to end of line
                    let actual_start_col = std::cmp::min(start_col_idx, chars.len());
                    result.push_str(&chars[actual_start_col..].iter().collect::<String>());
                } else if line_idx == actual_end_line_idx {
                    // Last line - from beginning to end_column
                    let actual_end_col = std::cmp::min(end_col_idx, chars.len());
                    result.push_str(&chars[..actual_end_col].iter().collect::<String>());
                } else {
                    // Middle lines - entire line
                    result.push_str(line);
                }

                // Add newline except for the last line
                if line_idx < actual_end_line_idx {
                    result.push('\n');
                }
            }

            result
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}-{}:{}", self.file, self.start_line, self.start_column, self.end_line, self.end_column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    fn create_temp_file(content: &str) -> (NamedTempFile, Location) {
        let mut temp_file = NamedTempFile::new().unwrap();
        temp_file.write_all(content.as_bytes()).unwrap();
        temp_file.flush().unwrap();

        let location = Location {
            file: temp_file.path().to_string_lossy().to_string(),
            start_line: 1,
            start_column: 1,
            end_line: 1,
            end_column: 1,
        };

        (temp_file, location)
    }

    #[test]
    fn test_single_line_extraction() {
        let content = "Hello World";
        let (_temp_file, mut location) = create_temp_file(content);

        // Extract "Hello"
        location.start_line = 1;
        location.start_column = 1;
        location.end_line = 1;
        location.end_column = 6;

        assert_eq!(location.source(), "Hello");

        // Extract "World"
        location.start_column = 7;
        location.end_column = 12;

        assert_eq!(location.source(), "World");
    }

    #[test]
    fn test_multi_line_extraction() {
        let content = "Line 1\nLine 2\nLine 3";
        let (_temp_file, mut location) = create_temp_file(content);

        // Extract from middle of line 1 to middle of line 3
        location.start_line = 1;
        location.start_column = 3;
        location.end_line = 3;
        location.end_column = 4;

        assert_eq!(location.source(), "ne 1\nLine 2\nLin");
    }

    #[test]
    fn test_entire_line_extraction() {
        let content = "Line 1\nLine 2\nLine 3";
        let (_temp_file, mut location) = create_temp_file(content);

        // Extract entire second line
        location.start_line = 2;
        location.start_column = 1;
        location.end_line = 2;
        location.end_column = 7;

        assert_eq!(location.source(), "Line 2");
    }

    #[test]
    fn test_empty_file() {
        let content = "";
        let (_temp_file, location) = create_temp_file(content);

        assert_eq!(location.source(), "");
    }

    #[test]
    fn test_utf8_characters() {
        let content = "Hello 🌍 World\nUTF-8 测试\n";
        let (_temp_file, mut location) = create_temp_file(content);

        // Extract emoji
        location.start_line = 1;
        location.start_column = 7;
        location.end_line = 1;
        location.end_column = 8;

        assert_eq!(location.source(), "🌍");

        // Extract Chinese characters
        location.start_line = 2;
        location.start_column = 7;
        location.end_line = 2;
        location.end_column = 9;

        assert_eq!(location.source(), "测试");
    }

    #[test]
    fn test_out_of_bounds_line() {
        let content = "Line 1\nLine 2";
        let (_temp_file, mut location) = create_temp_file(content);

        // Try to extract from non-existent line
        location.start_line = 5;
        location.start_column = 1;
        location.end_line = 5;
        location.end_column = 5;

        assert_eq!(location.source(), "");
    }

    #[test]
    fn test_out_of_bounds_column() {
        let content = "Short";
        let (_temp_file, mut location) = create_temp_file(content);

        // Try to extract beyond line length
        location.start_line = 1;
        location.start_column = 3;
        location.end_line = 1;
        location.end_column = 20;

        assert_eq!(location.source(), "ort");
    }

    #[test]
    fn test_zero_width_extraction() {
        let content = "Hello World";
        let (_temp_file, mut location) = create_temp_file(content);

        // Extract zero-width range
        location.start_line = 1;
        location.start_column = 5;
        location.end_line = 1;
        location.end_column = 5;

        assert_eq!(location.source(), "");
    }

    #[test]
    fn test_newline_handling() {
        let content = "Line 1\n\nLine 3";
        let (_temp_file, mut location) = create_temp_file(content);

        // Extract across empty line
        location.start_line = 1;
        location.start_column = 4;
        location.end_line = 3;
        location.end_column = 3;

        assert_eq!(location.source(), "e 1\n\nLi");
    }

    #[test]
    fn test_single_character_extraction() {
        let content = "abcd";
        let (_temp_file, mut location) = create_temp_file(content);

        // Extract single character
        location.start_line = 1;
        location.start_column = 2;
        location.end_line = 1;
        location.end_column = 3;

        assert_eq!(location.source(), "b");
    }
}