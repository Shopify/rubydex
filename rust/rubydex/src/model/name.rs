use std::fmt::Display;

use crate::model::ids::{NameId, StringId};

#[derive(Debug, Clone, Copy)]
pub enum ParentScope {
    /// There's no parent scope in this reference (e.g.: `Foo`)
    None,
    /// There's an empty parent scope in this reference (e.g.: `::Foo`)
    TopLevel,
    /// There's a parent scope in this reference (e.g.: `Foo::Bar`)
    Some(NameId),
}

impl ParentScope {
    pub fn map_or<F, T>(&self, default: T, f: F) -> T
    where
        F: FnOnce(&NameId) -> T,
    {
        match self {
            ParentScope::Some(id) => f(id),
            _ => default,
        }
    }

    #[must_use]
    pub fn as_ref(&self) -> Option<&NameId> {
        match self {
            ParentScope::Some(id) => Some(id),
            _ => None,
        }
    }

    #[must_use]
    pub fn is_none(&self) -> bool {
        matches!(self, ParentScope::None)
    }

    #[must_use]
    pub fn is_top_level(&self) -> bool {
        matches!(self, ParentScope::TopLevel)
    }

    /// # Panics
    ///
    /// Panics if the `ParentScope` is None or `TopLevel`
    #[must_use]
    pub fn expect(&self, message: &str) -> NameId {
        match self {
            ParentScope::Some(id) => *id,
            _ => panic!("{}", message),
        }
    }
}

impl Display for ParentScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParentScope::None => write!(f, "None"),
            ParentScope::TopLevel => write!(f, "TopLevel"),
            ParentScope::Some(id) => write!(f, "Some({id})"),
        }
    }
}

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
    parent_scope: ParentScope,
    /// The ID of the name for the nesting where we found this name. This effectively turns the structure into a linked
    /// list of names to represent the nesting
    nesting: Option<NameId>,
    ref_count: usize,
}

impl Name {
    #[must_use]
    pub fn new(str: StringId, parent_scope: ParentScope, nesting: Option<NameId>) -> Self {
        Self {
            str,
            parent_scope,
            nesting,
            ref_count: 1,
        }
    }

    #[must_use]
    pub fn str(&self) -> &StringId {
        &self.str
    }

    #[must_use]
    pub fn parent_scope(&self) -> &ParentScope {
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
            self.parent_scope,
            self.nesting.map_or(String::from("None"), |id| id.to_string())
        ))
    }

    #[must_use]
    pub fn ref_count(&self) -> usize {
        self.ref_count
    }

    pub fn increment_ref_count(&mut self, count: usize) {
        self.ref_count += count;
    }

    #[must_use]
    pub fn decrement_ref_count(&mut self) -> bool {
        self.ref_count -= 1;
        self.ref_count > 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn same_parent_scope_and_nesting() {
        let name_1 = Name::new(StringId::from("Foo"), ParentScope::None, None);
        let name_2 = Name::new(StringId::from("Foo"), ParentScope::None, None);
        assert_eq!(name_1.id(), name_2.id());

        let name_3 = Name::new(StringId::from("Foo"), ParentScope::Some(name_1.id()), None);
        let name_4 = Name::new(StringId::from("Foo"), ParentScope::Some(name_2.id()), None);
        assert_eq!(name_3.id(), name_4.id());

        let name_5 = Name::new(StringId::from("Foo"), ParentScope::None, Some(name_1.id()));
        let name_6 = Name::new(StringId::from("Foo"), ParentScope::None, Some(name_2.id()));
        assert_eq!(name_5.id(), name_6.id());
        assert_ne!(name_3.id(), name_5.id());
        assert_ne!(name_4.id(), name_6.id());

        let name_7 = Name::new(
            StringId::from("Foo"),
            ParentScope::Some(Name::new(StringId::from("Foo"), ParentScope::None, None).id()),
            Some(Name::new(StringId::from("Foo"), ParentScope::None, None).id()),
        );
        let name_8 = Name::new(
            StringId::from("Foo"),
            ParentScope::Some(Name::new(StringId::from("Foo"), ParentScope::None, None).id()),
            Some(Name::new(StringId::from("Foo"), ParentScope::None, None).id()),
        );
        assert_eq!(name_7.id(), name_8.id());
    }
}
