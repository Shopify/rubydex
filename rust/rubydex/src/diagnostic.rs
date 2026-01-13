use crate::{model::ids::UriId, offset::Offset};

#[derive(Debug)]
pub struct Diagnostic {
    code: u16,
    uri_id: UriId,
    offset: Offset,
    message: String,
}

impl Diagnostic {
    #[must_use]
    pub fn new(code: u16, uri_id: UriId, offset: Offset, message: String) -> Self {
        Self {
            code,
            uri_id,
            offset,
            message,
        }
    }

    #[must_use]
    pub fn make(diagnostics: Diagnostics, uri_id: UriId, offset: Offset, message: String) -> Self {
        Self::new(diagnostics.code(), uri_id, offset, message)
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
}

macro_rules! diagnostics {
    ( $( ($code:expr, $name:ident); )* ) => {
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
        }
    }
}

diagnostics! {
    // 0000 -> 1000 - Internal errors

    // 2000 - Parsing errors
    (2000, ParseError);
    (2001, ParseWarning);

    // 3000 - Indexing errors
    (3001, DynamicConstantReference);
    (3002, DynamicSingletonDefinition);
    (3003, DynamicAncestor);
    (3004, TopLevelMixinSelf);

    // 4000 - Resolution errors
}
