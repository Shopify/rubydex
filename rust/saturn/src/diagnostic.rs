use crate::{model::ids::UriId, offset::Offset};

#[derive(Debug)]
pub struct Diagnostic {
    uri_id: UriId,
    offset: Offset,
    message: String,
    severity: Severity,
}

impl Diagnostic {
    #[must_use]
    pub fn new(uri_id: UriId, offset: Offset, message: String, severity: Severity) -> Self {
        Self {
            uri_id,
            offset,
            message,
            severity,
        }
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        &self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn message(&self) -> &str {
        &self.message
    }

    #[must_use]
    pub fn severity(&self) -> &Severity {
        &self.severity
    }
}

#[derive(Debug)]
pub enum Severity {
    Error,
    Warning,
}

impl Severity {
    #[must_use]
    pub fn as_str(&self) -> &str {
        match self {
            Severity::Error => "Error",
            Severity::Warning => "Warning",
        }
    }
}
