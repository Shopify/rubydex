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

    pub(crate) fn set_severity(&mut self, severity: Severity) {
        self.severity = severity;
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    Error,
    Warning,
    Information,
    Hint,
}

macro_rules! rules {
    (
        $( $variant:ident => $name:literal );* $(;)?
    ) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub enum Rule {
            $(
                $variant,
            )*
        }

        impl Rule {
            /// All diagnostic rules emitted by the graph.
            pub const ALL: &[Self] = &[
                $(
                    Self::$variant,
                )*
            ];

            #[must_use]
            pub const fn name(self) -> &'static str {
                match self {
                    $(
                        Self::$variant => $name,
                    )*
                }
            }
        }

        impl std::fmt::Display for Rule {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(self.name())
            }
        }
    }
}

rules! {
    // Parsing
    ParseError => "parse-error";
    ParseWarning => "parse-warning";

    // Indexing
    DynamicConstantReference => "dynamic-constant-reference";
    DynamicSingletonDefinition => "dynamic-singleton-definition";
    DynamicAncestor => "dynamic-ancestor";
    TopLevelMixinSelf => "top-level-mixin-self";
    InvalidConstantVisibility => "invalid-constant-visibility";
    InvalidMethodVisibility => "invalid-method-visibility";

    // Resolution
    UndefinedMethodVisibilityTarget => "undefined-method-visibility-target";
    UndefinedConstantVisibilityTarget => "undefined-constant-visibility-target";
}
