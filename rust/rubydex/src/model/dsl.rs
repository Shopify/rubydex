//! DSL-related types for capturing DSL method calls during indexing.

use crate::model::ids::ReferenceId;
use crate::offset::Offset;

/// Represents a value in a DSL argument.
/// Can be either a raw string or a resolved constant reference.
#[derive(Debug, Clone)]
pub enum DslValue {
    /// A raw string value (e.g., `"123"`, `some_var`)
    String(String),
    /// A constant reference (e.g., `Foo`, `Bar::Baz`)
    Reference(ReferenceId),
}

impl DslValue {
    /// Returns the value as a string if it is a `String` variant.
    #[must_use]
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::String(s) => Some(s.as_str()),
            Self::Reference(_) => None,
        }
    }

    /// Returns the reference ID if it is a `Reference` variant.
    #[must_use]
    pub fn as_reference(&self) -> Option<ReferenceId> {
        match self {
            Self::Reference(id) => Some(*id),
            Self::String(_) => None,
        }
    }
}

/// Represents a single argument in a DSL call.
#[derive(Debug, Clone)]
pub enum DslArgument {
    /// A positional argument (e.g., `Parent` in `Class.new(Parent)`)
    Positional(DslValue),
    /// A keyword argument (e.g., `class_name: "User"`)
    KeywordArg { key: String, value: DslValue },
    /// A splat argument (e.g., `*args`)
    Splat(String),
    /// A double splat argument (e.g., `**kwargs`)
    DoubleSplat(String),
    /// A block argument (e.g., `&block`)
    BlockArg(String),
}

/// Represents the arguments passed to a DSL method call.
#[derive(Debug, Clone, Default)]
pub struct DslArgumentList {
    arguments: Vec<DslArgument>,
    block_offset: Option<Offset>,
}

impl DslArgumentList {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, arg: DslArgument) {
        self.arguments.push(arg);
    }

    pub fn set_block_offset(&mut self, offset: Offset) {
        self.block_offset = Some(offset);
    }

    #[must_use]
    pub fn arguments(&self) -> &[DslArgument] {
        &self.arguments
    }

    #[must_use]
    pub fn block_offset(&self) -> Option<&Offset> {
        self.block_offset.as_ref()
    }

    #[must_use]
    pub fn has_block(&self) -> bool {
        self.block_offset.is_some()
    }

    /// Returns the first positional argument as a reference ID if it is a constant reference.
    #[must_use]
    pub fn first_positional_reference(&self) -> Option<ReferenceId> {
        self.arguments.iter().find_map(|arg| match arg {
            DslArgument::Positional(value) => value.as_reference(),
            _ => None,
        })
    }

    #[must_use]
    pub fn positional_args(&self) -> Vec<&DslValue> {
        self.arguments
            .iter()
            .filter_map(|arg| match arg {
                DslArgument::Positional(v) => Some(v),
                _ => None,
            })
            .collect()
    }

    #[must_use]
    pub fn keyword_arg(&self, key: &str) -> Option<&DslValue> {
        self.arguments.iter().find_map(|arg| match arg {
            DslArgument::KeywordArg { key: k, value } if k == key => Some(value),
            _ => None,
        })
    }
}
