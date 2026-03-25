use core::fmt;
use std::fmt::Display;

use super::ids::{DefinitionId, StringId};
use crate::offset::Offset;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Protected,
    Private,
    ModuleFunction,
}

/// What a visibility directive does to its target.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VisibilityDirectiveKind {
    /// `private :foo` / `protected :foo` / `public :foo`
    SetInstanceMethodVisibility(Visibility),
}

/// A visibility directive recorded during discovery and executed during resolution.
/// For local members, mutates the declaration's visibility.
#[derive(Debug, Clone)]
pub struct VisibilityDirective {
    /// Unqualified name of the target member
    target_name: StringId,
    /// What this directive does
    kind: VisibilityDirectiveKind,
    /// The definition ID of the owning namespace. None at top-level (owner is Object).
    owner_definition_id: Option<DefinitionId>,
    /// Source location (for ordering and diagnostics)
    offset: Offset,
}

impl VisibilityDirective {
    #[must_use]
    pub fn new(
        target_name: StringId,
        kind: VisibilityDirectiveKind,
        owner_definition_id: Option<DefinitionId>,
        offset: Offset,
    ) -> Self {
        Self {
            target_name,
            kind,
            owner_definition_id,
            offset,
        }
    }

    #[must_use]
    pub fn target_name(&self) -> StringId {
        self.target_name
    }

    #[must_use]
    pub fn kind(&self) -> VisibilityDirectiveKind {
        self.kind
    }

    #[must_use]
    pub fn owner_definition_id(&self) -> Option<DefinitionId> {
        self.owner_definition_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }
}

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
