use std::error::Error;

#[derive(Debug)]
pub struct MultipleErrors(pub Vec<Errors>);

impl std::fmt::Display for MultipleErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Multiple errors: {:?}", self.0)
    }
}

impl Error for MultipleErrors {}

impl From<Errors> for MultipleErrors {
    fn from(error: Errors) -> Self {
        MultipleErrors(vec![error])
    }
}

// Enum representing all types of indexing errors that may happen
#[derive(Debug, Clone)]
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
