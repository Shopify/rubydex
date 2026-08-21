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
    ($($(#[doc = $documentation:literal])+ $rule:ident => $severity:ident),+ $(,)?) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub enum Rule {
            $($(#[doc = $documentation])+ $rule,)+
        }

        impl Rule {
            #[must_use]
            pub fn all() -> &'static [Self] {
                &[$(Self::$rule,)+]
            }

            #[must_use]
            pub fn name(&self) -> &'static str {
                match self {
                    $(Self::$rule => stringify!($rule),)+
                }
            }

            #[must_use]
            pub fn default_severity(&self) -> Severity {
                match self {
                    $(Self::$rule => Severity::$severity,)+
                }
            }

            #[must_use]
            pub fn documentation(&self) -> &'static [&'static str] {
                match self {
                    $(Self::$rule => &[$($documentation,)+],)+
                }
            }
        }
    };
}

rules! {
    // ******** Parsing ******** //

    /// A parse error represents invalid Ruby syntax and a program that will fail to execute. For example, a missing
    /// `end`, an unterminated string, a missing parenthesis.
    ParseError => Error,

    /// Parse warnings represent code that has valid syntax, but may not do what the developer expects. For example,
    /// local variables that are completely unused or usage in a void context (creating a line of code that does
    /// nothing).
    ///
    /// ```ruby
    /// CONST = 1
    /// CONST # <<< void context
    /// puts CONST + 2
    /// ```
    ParseWarning => Warning,

    // ******** Indexing ******** //

    /// A dynamic constant reference usage is not necessarily a mistake, but Rubydex cannot reason about it due to its
    /// dynamic nature. For example, `var::Foo`. It's not possible to determine what `Foo` is being referred to because
    /// `var` is a value that depends on the runtime. Using this type of pattern will degrade the quality of the
    /// analysis as Rubydex might be missing some information about how the code truly behaves.
    DynamicConstantReference => Information,

    /// A dynamic singleton block target is not necessarily a mistake, but Rubydex cannot reason about it due to its
    /// dynamic nature. For example, in `class << var` or `def var.foo`, it's not possible to determine statically what
    /// is being defined since `var` is a value that depends on the runtime. Using this type of pattern will degrade the
    /// quality of the analysis as Rubydex might be missing some information about how the code truly behaves.
    DynamicSingletonDefinition => Information,

    /// A dynamic ancestor is not necessarily a mistake, but Rubydex cannot reason about it due to its dynamic nature.
    /// For example, in `class Child < var` or `include var`, it's not possible to determine statically what ancestor is
    /// being used since `var` is a value that depends on the runtime. Using this type of pattern will degrade the
    /// quality of the analysis as Rubydex might be missing some information about how the code truly behaves.
    DynamicAncestor => Information,

    /// The top level of a Ruby program is the special <main> object. It is not possible to use `include self` or
    /// `extend self` on that object.
    TopLevelMixinSelf => Information,

    /// Reports the usage of a constant visibility operation (`public_constant`, `private_constant`) where either the
    /// receiver or the arguments are dynamic and cannot be analyzed statically. For example, in `public_constant(var)`
    /// or `var.public_constant(:Foo)`, it's not possible to determine statically what constant is being changed since
    /// `var` is a value that depends on the runtime. Using this type of pattern will degrade the quality of the
    /// analysis as Rubydex might be missing some information about how the code truly behaves.
    InvalidConstantVisibility => Warning,

    /// Reports a method visibility call that cannot be applied.
    ///
    /// `private`, `public`, `protected` and `module_function` need an enclosing namespace and literal arguments.
    /// Calling one at the top level or with a computed argument leaves the visibility of the methods unchanged. A
    /// wrapped `attr_*` call (`private attr_reader :foo`) is only understood when it is the single argument.
    InvalidMethodVisibility => Warning,

    // ******** Resolution ******** //

    /// Reports a method visibility operation naming a method that doesn't exist in the namespace. For example, `private
    /// :foo`, where `foo` is not defined anywhere in the ancestor chain of the owner.
    UndefinedMethodVisibilityTarget => Warning,

    /// Reports a constant visibility change naming a constant the namespace does not have.
    ///
    /// The call itself was understood and its target namespace resolved, but no constant by that name was ever defined
    /// there, so there is nothing to change. This is usually a typo or a leftover from a constant that was removed.
    UndefinedConstantVisibilityTarget => Warning,
}

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}
