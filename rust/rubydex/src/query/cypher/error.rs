use std::fmt;

/// An error produced while lexing, parsing, or executing a Cypher query.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CypherError {
    /// A lexing or parsing error, with a byte position into the source query.
    Syntax { message: String, position: usize },
    /// A semantic or execution error (e.g. unknown property, type mismatch).
    Execution { message: String },
}

impl CypherError {
    pub(crate) fn syntax(message: impl Into<String>, position: usize) -> Self {
        Self::Syntax {
            message: message.into(),
            position,
        }
    }

    pub(crate) fn execution(message: impl Into<String>) -> Self {
        Self::Execution {
            message: message.into(),
        }
    }
}

impl fmt::Display for CypherError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CypherError::Syntax { message, position } => {
                write!(f, "Cypher syntax error at position {position}: {message}")
            }
            CypherError::Execution { message } => {
                write!(f, "Cypher execution error: {message}")
            }
        }
    }
}

impl std::error::Error for CypherError {}
