use std::error::Error;

#[derive(Debug)]
pub struct MultipleErrors(pub Vec<IndexingError>);

impl std::fmt::Display for MultipleErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Multiple errors: {:?}", self.0)
    }
}

impl Error for MultipleErrors {}

impl From<Box<dyn Error>> for MultipleErrors {
    fn from(error: Box<dyn Error>) -> Self {
        MultipleErrors(vec![IndexingError::FailedDbOperation(error.to_string())])
    }
}

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
    FailedDbOperation(String),
}

impl std::fmt::Display for IndexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IndexingError::FileReadError(msg) => write!(f, "File read error: {msg}"),
            IndexingError::InvalidUri(msg) => write!(f, "Invalid URI: {msg}"),
            IndexingError::FailedDbOperation(msg) => write!(f, "Db operation failed: {msg}"),
        }
    }
}

impl Error for IndexingError {}
