use crate::{model::ids::UriId, offset::Offset};

macro_rules! codes {
    (
        $(
            ($code:expr, $variant:ident, $severity:expr)
        );* $(;)?
    ) => {
        #[derive(Debug)]
        pub enum Code {
            $(
                $variant,
            )*
        }

        impl Code {
            #[must_use]
            pub fn code(&self) -> u32 {
                match self {
                    $(
                        Code::$variant => $code,
                    )*
                }
            }

            #[must_use]
            pub fn severity(&self) -> Severity {
                match self {
                    $(
                        Code::$variant => $severity,
                    )*
                }
            }
        }
    };
}

codes! {
    // 1000 - Files listing errors
    (1000, PathError, Severity::Error);
    (1001, FileReadError, Severity::Error);

    // 2000 - Parsing errors
    (2000, ParseError, Severity::Error);
    (2001, ParseWarning, Severity::Warning);

    // 3000 - Indexing errors

    // 4000 - Resolution errors
}

#[derive(Debug)]
pub struct Diagnostic {
    code: u32,
    uri_id: UriId,
    offset: Offset,
    message: String,
    severity: Severity,
}

impl Diagnostic {
    #[must_use]
    pub fn new(code: u32, uri_id: UriId, offset: Offset, message: String, severity: Severity) -> Self {
        Self {
            code,
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

    #[must_use]
    pub fn code(&self) -> u32 {
        self.code
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
