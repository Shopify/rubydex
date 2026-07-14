use std::fmt::Display;

use crate::{
    assert_mem_size,
    model::ids::{DeclarationId, NameId, StringId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParentScope {
    /// There's no parent scope in this reference (e.g.: `Foo`)
    None,
    /// There's an empty parent scope in this reference (e.g.: `::Foo`)
    TopLevel,
    /// There's a parent scope in this reference (e.g.: `Foo::Bar`)
    Some(NameId),
    /// Parent scope representing the class attached to a singleton class context
    ///
    ///  `Foo::<Foo>::<<Foo>>`
    ///  ^ Attached for <Foo>
    ///        ^ Attached for <<Foo>>
    Attached(NameId),
}
assert_mem_size!(ParentScope, 16);

impl ParentScope {
    pub fn map_or<F, T>(&self, default: T, f: F) -> T
    where
        F: FnOnce(&NameId) -> T,
    {
        match self {
            ParentScope::Some(id) | ParentScope::Attached(id) => f(id),
            _ => default,
        }
    }

    #[must_use]
    pub fn as_ref(&self) -> Option<&NameId> {
        match self {
            ParentScope::Some(id) | ParentScope::Attached(id) => Some(id),
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
            ParentScope::Some(id) | ParentScope::Attached(id) => *id,
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
            ParentScope::Attached(id) => write!(f, "Attached({id})"),
        }
    }
}

#[derive(Debug, Clone, Eq)]
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
    ref_count: u32,
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.str == other.str && self.parent_scope == other.parent_scope && self.nesting == other.nesting
    }
}
assert_mem_size!(Name, 40);

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
        let str_bytes = self.str.get().to_le_bytes();

        // We need to include both the variant and the bytes of the parent scope when present
        let (parent_tag, parent_bytes): (u8, [u8; 8]) = match self.parent_scope {
            ParentScope::None => (0, [0; 8]),
            ParentScope::TopLevel => (1, [0; 8]),
            ParentScope::Some(id) => (2, id.get().to_le_bytes()),
            ParentScope::Attached(id) => (3, id.get().to_le_bytes()),
        };
        let (nesting_tag, nesting_bytes): (u8, [u8; 8]) = match self.nesting {
            None => (0, [0; 8]),
            Some(id) => (1, id.get().to_le_bytes()),
        };

        NameId::from([
            str_bytes.as_slice(),
            [parent_tag].as_slice(),
            parent_bytes.as_slice(),
            [nesting_tag].as_slice(),
            nesting_bytes.as_slice(),
        ])
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedName {
    name: Name,
    declaration_id: DeclarationId,
}
assert_mem_size!(ResolvedName, 48);

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

    #[must_use]
    pub fn nesting(&self) -> &Option<NameId> {
        self.name.nesting()
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
assert_mem_size!(NameRef, 16);

impl NameRef {
    #[must_use]
    pub fn str(&self) -> &StringId {
        match self {
            NameRef::Unresolved(name) => name.str(),
            NameRef::Resolved(resolved_name) => resolved_name.name.str(),
        }
    }

    #[must_use]
    pub fn parent_scope(&self) -> &ParentScope {
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

    #[must_use]
    pub fn ref_count(&self) -> u32 {
        match self {
            NameRef::Unresolved(name) => name.ref_count,
            NameRef::Resolved(resolved_name) => resolved_name.name.ref_count,
        }
    }

    /// # Panics
    ///
    /// Panics if we exceed the maximum size of the reference count
    pub fn increment_ref_count(&mut self, count: u32) {
        let ref_count = match self {
            NameRef::Unresolved(name) => &mut name.ref_count,
            NameRef::Resolved(resolved_name) => &mut resolved_name.name.ref_count,
        };
        *ref_count = ref_count
            .checked_add(count)
            .expect("Should not exceed maximum name ref count");
    }

    #[must_use]
    pub fn decrement_ref_count(&mut self) -> bool {
        match self {
            NameRef::Unresolved(name) => {
                name.ref_count -= 1;
                name.ref_count > 0
            }
            NameRef::Resolved(resolved_name) => {
                resolved_name.name.ref_count -= 1;
                resolved_name.name.ref_count > 0
            }
        }
    }
}

impl PartialEq for NameRef {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (NameRef::Unresolved(a), NameRef::Unresolved(b)) => a == b,
            (NameRef::Resolved(a), NameRef::Resolved(b)) => a.name == b.name,
            (NameRef::Unresolved(name), NameRef::Resolved(resolved))
            | (NameRef::Resolved(resolved), NameRef::Unresolved(name)) => **name == resolved.name,
        }
    }
}

impl PartialEq<Name> for NameRef {
    fn eq(&self, other: &Name) -> bool {
        match self {
            NameRef::Unresolved(name) => **name == *other,
            NameRef::Resolved(resolved_name) => &resolved_name.name == other,
        }
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

    #[test]
    fn parent_scope_variants_are_distinct() {
        let inner = Name::new(StringId::from("Foo"), ParentScope::None, None).id();

        let ids = [
            Name::new(StringId::from("Foo"), ParentScope::None, None).id(),
            Name::new(StringId::from("Foo"), ParentScope::TopLevel, None).id(),
            Name::new(StringId::from("Foo"), ParentScope::Some(inner), None).id(),
            Name::new(StringId::from("Foo"), ParentScope::Attached(inner), None).id(),
        ];

        for (i, a) in ids.iter().enumerate() {
            for b in &ids[i + 1..] {
                assert_ne!(a, b);
            }
        }
    }
}
