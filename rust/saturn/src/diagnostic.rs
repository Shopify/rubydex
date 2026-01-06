use crate::{model::ids::UriId, offset::Offset};

#[derive(Debug)]
pub struct Diagnostic {
    code: u16,
    uri_id: UriId,
    offset: Offset,
    message: String,
    severity: Severity,
}

impl Diagnostic {
    #[must_use]
    pub fn new(code: u16, uri_id: UriId, offset: Offset, message: String, severity: Severity) -> Self {
        Self {
            code,
            uri_id,
            offset,
            message,
            severity,
        }
    }

    #[must_use]
    pub fn make(diagnostics: Diagnostics, uri_id: UriId, offset: Offset, message: String) -> Self {
        Self::new(diagnostics.code(), uri_id, offset, message, diagnostics.severity())
    }

    #[must_use]
    pub fn code(&self) -> u16 {
        self.code
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

macro_rules! diagnostics {
    ( $( ($code:expr, $name:ident, $severity:expr); )* ) => {
        #[derive(Debug, Copy, Clone)]
        pub enum Diagnostics {
            $(
                $name,
            )*
        }

        impl Diagnostics {
            pub fn code(&self) -> u16 {
                match self {
                $(
                    Diagnostics::$name => $code,
                )*
                }
            }

            pub fn severity(&self) -> Severity {
                match self {
                    $(
                        Diagnostics::$name => $severity,
                    )*
                }
            }
        }
    }
}

diagnostics! {
    // 0000 -> 1000 - Internal errors

    // 2000 - Parsing errors
    (2000, ParseError, Severity::Error);
    (2001, ParseWarning, Severity::Warning);

    // 3000 - Indexing errors

    // 4000 - Resolution errors
}
