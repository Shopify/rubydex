use std::error::Error;

#[derive(Debug)]
pub struct MultipleErrors(pub Vec<IndexingError>);

impl std::fmt::Display for MultipleErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Multiple errors: {:?}", self.0)
    }
}

impl Error for MultipleErrors {}

impl From<IndexingError> for MultipleErrors {
    fn from(error: IndexingError) -> Self {
        MultipleErrors(vec![error])
    }
}

// Enum representing all types of indexing errors that may happen
#[derive(Debug)]
pub enum IndexingError {
    FileReadError(String),
    InvalidUri(String),
    DatabaseError(String),
}

impl std::fmt::Display for IndexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IndexingError::FileReadError(msg) => write!(f, "File read error: {msg}"),
            IndexingError::InvalidUri(msg) => write!(f, "Invalid URI: {msg}"),
            IndexingError::DatabaseError(msg) => write!(f, "Database error: {msg}"),
        }
    }
}

impl Error for IndexingError {}
