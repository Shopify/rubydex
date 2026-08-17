#[cfg(any(test, feature = "test_utils"))]
use crate::model::document::Document;
use crate::{assert_mem_size, model::ids::UriId, offset::Offset};

#[derive(Debug)]
pub struct Diagnostic {
    rule: Rule,
    uri_id: UriId,
    offset: Offset,
    message: String,
}
assert_mem_size!(Diagnostic, 48);

impl Diagnostic {
    #[must_use]
    pub fn new(rule: Rule, uri_id: UriId, offset: Offset, message: String) -> Self {
        Self {
            rule,
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
    ($($(#[$attribute:meta])* $rule:ident),+ $(,)?) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub enum Rule {
            $($(#[$attribute])* $rule,)+
        }

        impl Rule {
            #[must_use]
            pub fn all() -> &'static [Self] {
                &[$(Self::$rule,)+]
            }
        }
    };
}

rules! {
    // Parsing
    ParseError,
    ParseWarning,

    // Indexing
    DynamicConstantReference,
    DynamicSingletonDefinition,
    DynamicAncestor,
    TopLevelMixinSelf,
    InvalidConstantVisibility,
    InvalidMethodVisibility,

    // Resolution
    UndefinedMethodVisibilityTarget,
    UndefinedConstantVisibilityTarget,
}

impl Rule {
    #[must_use]
    pub fn name(&self) -> &'static str {
        match self {
            Self::ParseError => "ParseError",
            Self::ParseWarning => "ParseWarning",
            Self::DynamicConstantReference => "DynamicConstantReference",
            Self::DynamicSingletonDefinition => "DynamicSingletonDefinition",
            Self::DynamicAncestor => "DynamicAncestor",
            Self::TopLevelMixinSelf => "TopLevelMixinSelf",
            Self::InvalidConstantVisibility => "InvalidConstantVisibility",
            Self::InvalidMethodVisibility => "InvalidMethodVisibility",
            Self::UndefinedMethodVisibilityTarget => "UndefinedMethodVisibilityTarget",
            Self::UndefinedConstantVisibilityTarget => "UndefinedConstantVisibilityTarget",
        }
    }

    #[must_use]
    pub fn default_severity(&self) -> Severity {
        match self {
            Self::ParseError => Severity::Error,
            Self::ParseWarning
            | Self::InvalidConstantVisibility
            | Self::InvalidMethodVisibility
            | Self::UndefinedMethodVisibilityTarget
            | Self::UndefinedConstantVisibilityTarget => Severity::Warning,
            Self::DynamicConstantReference
            | Self::DynamicSingletonDefinition
            | Self::DynamicAncestor
            | Self::TopLevelMixinSelf => Severity::Information,
        }
    }
}

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}
