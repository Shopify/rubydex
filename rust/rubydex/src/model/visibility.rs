use core::fmt;
use std::fmt::Display;

use crate::assert_mem_size;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Protected,
    Private,
    ModuleFunction,
}
assert_mem_size!(Visibility, 1);

impl Visibility {
    /// Parse a visibility from a string.
    ///
    /// Valid values are `public`, `protected`, and `private`.
    ///
    /// # Panics
    ///
    /// Panics if the string is not a valid visibility
    #[must_use]
    pub fn from_string(str: &str) -> Self {
        match str {
            "public" => Self::Public,
            "protected" => Self::Protected,
            "private" => Self::Private,
            "module_function" => Self::ModuleFunction,
            _ => panic!("Invalid visibility: {str}"),
        }
    }
}

/// These methods are implicitly private except when there is an inline modifier.
#[must_use]
pub fn is_implicitly_private_instance_method(name: &str) -> bool {
    matches!(
        name,
        "initialize" | "initialize_copy" | "initialize_clone" | "initialize_dup" | "respond_to_missing?"
    )
}

impl Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Public => write!(f, "public"),
            Self::Protected => write!(f, "protected"),
            Self::Private => write!(f, "private"),
            Self::ModuleFunction => write!(f, "module_function"),
        }
    }
}
