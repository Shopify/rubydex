// TODO: Fix import path issue - temporarily commenting out
use crate::tables::global_tables::{FileId, intern_file, with_file};

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    pub file_id: FileId, // Memory-efficient file reference using global table
    pub start_offset: usize,
    pub end_offset: usize,
}

impl Location {
    pub fn new(file: String, start_offset: usize, end_offset: usize) -> Self {
        let file_id = intern_file(&file);
        Self { file_id, start_offset, end_offset }
    }

    pub fn new_with_id(file_id: FileId, start_offset: usize, end_offset: usize) -> Self {
        Self { file_id, start_offset, end_offset }
    }

    /// Get the file path as a string
    pub fn file(&self) -> Option<String> {
        with_file(self.file_id, |s| s.to_string())
    }

    /// Execute a function with the file path
    pub fn with_file<R>(&self, f: impl FnOnce(&str) -> R) -> Option<R> {
        with_file(self.file_id, f)
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.file() {
            Some(file) => write!(f, "{}:{}-{}", file, self.start_offset, self.end_offset),
            None => write!(f, "<unknown>:{}-{}", self.start_offset, self.end_offset),
        }
    }
}

