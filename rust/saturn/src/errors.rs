use std::error::Error;

// Enum representing all types of indexing errors that may happen
#[derive(Debug, PartialEq, Eq)]
pub enum Errors {
    FileReadError(String),
    InvalidUri(String),
}

impl std::fmt::Display for Errors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Errors::FileReadError(msg) => write!(f, "File read error: {msg}"),
            Errors::InvalidUri(msg) => write!(f, "Invalid URI: {msg}"),
        }
    }
}

impl Error for Errors {}
