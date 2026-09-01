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
    ///
    /// ```ruby
    /// class Foo
    /// ^^^^^ Syntax error. Missing end token
    /// ```
    ParseError => Error,

    /// Parse warnings represent code that has valid syntax, but may not do what the developer expects. For example,
    /// local variables that are completely unused or usage in a void context (creating a line of code that does
    /// nothing).
    ///
    /// ```ruby
    /// CONST = 1
    /// CONST
    /// ^^^^^ Constant used in void context (the expression does nothing).
    /// puts CONST + 2
    /// ```
    ParseWarning => Warning,

    // ******** Indexing ******** //

    /// Dynamic constant references cannot be reasoned about statically because they depend on runtime values. The
    /// program may still be valid, but the quality of the analysis degrades.
    ///
    /// ```ruby
    /// var::Foo
    /// ^^^^^^^^ Dynamic constant reference. This might be correct, but it cannot be understood by the analysis.
    /// ```
    DynamicConstantReference => Information,

    /// Dynamic singleton targets cannot be reasoned about statically because they depend on runtime values. The program
    /// may still be valid, but the quality of the analysis degrades.
    ///
    /// ```ruby
    /// class << var
    ///          ^^^ Dynamic singleton target. This might be correct, but it cannot be understood by the analysis.
    /// end
    ///
    /// def var.bar
    ///     ^^^ Dynamic singleton target.
    /// end
    /// ```
    DynamicSingletonDefinition => Information,

    /// Dynamic ancestor references cannot be reasoned about statically because they depend on runtime values. The
    /// program may still be valid, but the quality of the analysis degrades. In the case of ancestors, since they
    /// influence constant resolution, the degradation may be more impactful than other dynamic references.
    ///
    /// ```ruby
    /// class Foo < var
    ///             ^^^ Dynamic ancestor reference. This might be correct, but it cannot be understood by the analysis.
    ///   include SomeClass.method_call
    ///           ^^^^^^^^^^^^^^^^^^^^^ Dynamic ancestor reference.
    /// end
    /// ```
    DynamicAncestor => Information,

    /// The top level of a Ruby program is the special <main> object. It is not possible to use `include self` or
    /// `extend self` on that object.
    ///
    /// ```ruby
    /// include self
    ///         ^^^^ Cannot include self at the top level
    /// extend self
    ///        ^^^^ Cannot extend self at the top level
    /// ```
    TopLevelMixinSelf => Information,

    /// Constant visibility operations that depend on dynamic values cannot be reasoned about statically because they
    /// depend on runtime values. The program may still be valid, but the quality of the analysis degrades.
    ///
    /// ```ruby
    /// var.private_constant :Foo
    /// ^^^ Invalid constant visibility. This might be correct, but it cannot be understood by the analysis.
    ///
    /// private_constant(var)
    ///                  ^^^ Invalid constant visibility.
    /// ```
    InvalidConstantVisibility => Warning,

    /// Method visibility operations that depend on dynamic values cannot be reasoned about statically because they
    /// depend on runtime values. The program may still be valid, but the quality of the analysis degrades.
    ///
    /// ```ruby
    /// var.private :foo
    /// ^^^ Invalid method visibility. This might be correct, but it cannot be understood by the analysis.
    ///
    /// private(var)
    ///         ^^^ Invalid method visibility.
    /// ```
    InvalidMethodVisibility => Warning,

    // ******** Resolution ******** //

    /// Undefined method visibility target means the analysis couldn't find the definition for the method the code is
    /// attempting to change visibility of. It could be defined through meta-programming or it indeed does not exist.
    ///
    /// ```ruby
    /// class Foo
    ///   private :bar
    ///            ^^^ Undefined method visibility target. The method `bar` is not defined.
    /// end
    /// ```
    UndefinedMethodVisibilityTarget => Warning,

    /// Undefined constant visibility target means the analysis couldn't find the definition for the constant the code is
    /// attempting to change visibility of. It could be defined through meta-programming or it indeed does not exist.
    ///
    /// ```ruby
    /// class Foo
    ///   private_constant :Bar
    ///                    ^^^ Undefined constant visibility target. The constant `Bar` is not defined.
    /// end
    /// ```
    UndefinedConstantVisibilityTarget => Warning,
}

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}
