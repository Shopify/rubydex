#[cfg(any(test, feature = "test_utils"))]
use crate::model::document::Document;
use crate::{model::ids::UriId, offset::Offset};

#[derive(Debug)]
pub struct Diagnostic {
    rule: Rule,
    // Severity belongs to each diagnostic; every producer must assign it explicitly.
    severity: Severity,
    uri_id: UriId,
    offset: Offset,
    message: String,
}

impl Diagnostic {
    #[must_use]
    pub fn new(rule: Rule, severity: Severity, uri_id: UriId, offset: Offset, message: String) -> Self {
        Self {
            rule,
            severity,
            uri_id,
            offset,
            message,
        }
    }

    #[must_use]
    pub fn rule(&self) -> &Rule {
        &self.rule
    }

    #[must_use]
    pub fn severity(&self) -> &Severity {
        &self.severity
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

    #[cfg(any(test, feature = "test_utils"))]
    #[must_use]
    pub fn formatted(&self, document: &Document) -> String {
        format!(
            "{}: {} ({})",
            self.rule(),
            self.message(),
            self.offset().to_display_range(document)
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Information,
    Hint,
}

fn camel_to_snake(s: &str) -> String {
    let mut snake = String::new();
    for (i, ch) in s.chars().enumerate() {
        if ch.is_uppercase() {
            if i != 0 {
                snake.push('-');
            }
            for lc in ch.to_lowercase() {
                snake.push(lc);
            }
        } else {
            snake.push(ch);
        }
    }
    snake
}

macro_rules! rules {
    (
        $( $variant:ident );* $(;)?
    ) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub enum Rule {
            $(
                $variant,
            )*
        }

        impl std::fmt::Display for Rule {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", match self {
                    $(
                        Rule::$variant => camel_to_snake(stringify!($variant)),
                    )*
                })
            }
        }
    }
}

rules! {
    // Parsing
    ParseError;
    ParseWarning;

    // Indexing
    DynamicConstantReference;
    DynamicSingletonDefinition;
    DynamicAncestor;
    TopLevelMixinSelf;
    InvalidConstantVisibility;
    InvalidMethodVisibility;

    // Resolution
    UndefinedMethodVisibilityTarget;
    UndefinedConstantVisibilityTarget;
}
