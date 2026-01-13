use crate::model::ids::{DeclarationId, NameId, StringId};

#[derive(Debug, Clone)]
pub struct Name {
    /// The unqualified name of the constant
    str: StringId,
    /// The ID of parent scope for this constant. For example:
    ///
    /// ```ruby
    /// Foo::Bar::Baz
    /// # ^ parent scope of Bar::Baz
    /// #      ^ parent scope of Baz
    /// ```
    ///
    /// `None` indicates that this is a simple constant read or a top level reference
    parent_scope: Option<NameId>,
    /// The ID of the name for the nesting where we found this name. This effectively turns the structure into a linked
    /// list of names to represent the nesting
    nesting: Option<NameId>,
}

impl Name {
    #[must_use]
    pub fn new(str: StringId, parent_scope: Option<NameId>, nesting: Option<NameId>) -> Self {
        Self {
            str,
            parent_scope,
            nesting,
        }
    }

    #[must_use]
    pub fn str(&self) -> &StringId {
        &self.str
    }

    #[must_use]
    pub fn parent_scope(&self) -> &Option<NameId> {
        &self.parent_scope
    }

    #[must_use]
    pub fn nesting(&self) -> &Option<NameId> {
        &self.nesting
    }

    #[must_use]
    pub fn id(&self) -> NameId {
        NameId::from(&format!(
            "{}{}{}",
            self.str,
            self.parent_scope.map_or(String::from("None"), |id| id.to_string()),
            self.nesting.map_or(String::from("None"), |id| id.to_string())
        ))
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedName {
    name: Name,
    declaration_id: DeclarationId,
}

impl ResolvedName {
    #[must_use]
    pub fn new(name: Name, declaration_id: DeclarationId) -> Self {
        Self { name, declaration_id }
    }

    #[must_use]
    pub fn name(&self) -> &Name {
        &self.name
    }

    #[must_use]
    pub fn declaration_id(&self) -> &DeclarationId {
        &self.declaration_id
    }
}

/// A usage of a constant name. This could be a constant reference or a definition like a class or module
#[derive(Debug, Clone)]
pub enum NameRef {
    /// This name has not yet been resolved. We don't yet know what this name refers to or if it refers to an existing
    /// declaration
    Unresolved(Box<Name>),
    /// This name has been resolved to an existing declaration
    Resolved(Box<ResolvedName>),
}

impl NameRef {
    #[must_use]
    pub fn str(&self) -> &StringId {
        match self {
            NameRef::Unresolved(name) => name.str(),
            NameRef::Resolved(resolved_name) => resolved_name.name.str(),
        }
    }

    #[must_use]
    pub fn parent_scope(&self) -> &Option<NameId> {
        match self {
            NameRef::Unresolved(name) => name.parent_scope(),
            NameRef::Resolved(resolved_name) => resolved_name.name.parent_scope(),
        }
    }

    #[must_use]
    pub fn into_unresolved(self) -> Option<Name> {
        match self {
            NameRef::Unresolved(name) => Some(*name),
            NameRef::Resolved(_) => None,
        }
    }

    #[must_use]
    pub fn nesting(&self) -> &Option<NameId> {
        match self {
            NameRef::Unresolved(name) => name.nesting(),
            NameRef::Resolved(resolved_name) => resolved_name.name.nesting(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn same_parent_scope_and_nesting() {
        let name_1 = Name::new(StringId::from("Foo"), None, None);
        let name_2 = Name::new(StringId::from("Foo"), None, None);
        assert_eq!(name_1.id(), name_2.id());

        let name_3 = Name::new(StringId::from("Foo"), Some(name_1.id()), None);
        let name_4 = Name::new(StringId::from("Foo"), Some(name_2.id()), None);
        assert_eq!(name_3.id(), name_4.id());

        let name_5 = Name::new(StringId::from("Foo"), None, Some(name_1.id()));
        let name_6 = Name::new(StringId::from("Foo"), None, Some(name_2.id()));
        assert_eq!(name_5.id(), name_6.id());
        assert_ne!(name_3.id(), name_5.id());
        assert_ne!(name_4.id(), name_6.id());

        let name_7 = Name::new(
            StringId::from("Foo"),
            Some(Name::new(StringId::from("Foo"), None, None).id()),
            Some(Name::new(StringId::from("Foo"), None, None).id()),
        );
        let name_8 = Name::new(
            StringId::from("Foo"),
            Some(Name::new(StringId::from("Foo"), None, None).id()),
            Some(Name::new(StringId::from("Foo"), None, None).id()),
        );
        assert_eq!(name_7.id(), name_8.id());
    }
}
