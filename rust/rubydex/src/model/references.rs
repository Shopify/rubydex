use crate::{
    assert_mem_size,
    model::ids::{
        ConstantReferenceId, DeclarationId, DefinitionId, InstanceVariableReferenceId, MethodReferenceId, NameId,
        StringId, UriId,
    },
    offset::Offset,
};

/// A reference to a constant
#[derive(Debug)]
pub struct ConstantReference {
    /// The name ID of this reference
    name_id: NameId,
    /// The document where we found the reference
    uri_id: UriId,
    /// The offsets inside of the document where we found the reference
    offset: Offset,
}
assert_mem_size!(ConstantReference, 24);

impl ConstantReference {
    #[must_use]
    pub fn new(name_id: NameId, uri_id: UriId, offset: Offset) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
        }
    }

    #[must_use]
    pub fn name_id(&self) -> &NameId {
        &self.name_id
    }

    #[must_use]
    pub fn uri_id(&self) -> UriId {
        self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn id(&self) -> ConstantReferenceId {
        ConstantReferenceId::from(&format!(
            "{}:{}:{}-{}",
            self.name_id,
            self.uri_id,
            self.offset.start(),
            self.offset.end()
        ))
    }
}

/// A reference to a method
#[derive(Debug)]
pub struct MethodRef {
    /// The unqualified name of the method
    str: StringId,
    /// The document where we found the reference
    uri_id: UriId,
    /// The offsets inside of the document where we found the reference
    offset: Offset,
    /// The receiver of the method call if it's a constant
    receiver: Option<NameId>,
}
assert_mem_size!(MethodRef, 32);

impl MethodRef {
    #[must_use]
    pub fn new(str: StringId, uri_id: UriId, offset: Offset, receiver: Option<NameId>) -> Self {
        Self {
            str,
            uri_id,
            offset,
            receiver,
        }
    }

    #[must_use]
    pub fn str(&self) -> &StringId {
        &self.str
    }

    #[must_use]
    pub fn uri_id(&self) -> UriId {
        self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn receiver(&self) -> Option<NameId> {
        self.receiver
    }

    #[must_use]
    pub fn id(&self) -> MethodReferenceId {
        MethodReferenceId::from(&format!(
            "{}:{}:{}-{}",
            self.str,
            self.uri_id,
            self.offset.start(),
            self.offset.end()
        ))
    }
}

/// A reference to an instance variable
#[derive(Debug)]
pub struct InstanceVariableRef {
    /// The interned name of the instance variable (e.g., "@foo")
    str_id: StringId,
    /// The document where we found the reference
    uri_id: UriId,
    /// The offsets inside of the document where we found the reference
    offset: Offset,
    /// The lexical nesting at the point of the read, used to determine the owner during resolution
    lexical_nesting_id: Option<DefinitionId>,
}
assert_mem_size!(InstanceVariableRef, 32);

impl InstanceVariableRef {
    #[must_use]
    pub fn new(str_id: StringId, uri_id: UriId, offset: Offset, lexical_nesting_id: Option<DefinitionId>) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            lexical_nesting_id,
        }
    }

    #[must_use]
    pub fn str_id(&self) -> &StringId {
        &self.str_id
    }

    #[must_use]
    pub fn uri_id(&self) -> UriId {
        self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> Option<DefinitionId> {
        self.lexical_nesting_id
    }

    #[must_use]
    pub fn id(&self) -> InstanceVariableReferenceId {
        InstanceVariableReferenceId::from(&format!(
            "{}:{}:{}-{}",
            self.str_id,
            self.uri_id,
            self.offset.start(),
            self.offset.end()
        ))
    }
}

/// An instance variable reference that has been resolved to its target declaration.
#[derive(Debug)]
pub struct ResolvedInstanceVariableRef {
    inner: InstanceVariableRef,
    declaration_id: DeclarationId,
}
assert_mem_size!(ResolvedInstanceVariableRef, 40);

impl ResolvedInstanceVariableRef {
    #[must_use]
    pub fn new(inner: InstanceVariableRef, declaration_id: DeclarationId) -> Self {
        Self { inner, declaration_id }
    }

    #[must_use]
    pub fn inner(&self) -> &InstanceVariableRef {
        &self.inner
    }

    #[must_use]
    pub fn declaration_id(&self) -> &DeclarationId {
        &self.declaration_id
    }
}

/// A usage of an instance variable. Mirrors `NameRef`: the resolved variant carries the
/// `DeclarationId` directly so that going from a reference to its target declaration is O(1).
#[derive(Debug)]
pub enum InstanceVariableReference {
    /// Not yet linked to a declaration.
    Unresolved(Box<InstanceVariableRef>),
    /// Linked to a declaration in the graph.
    Resolved(Box<ResolvedInstanceVariableRef>),
}
assert_mem_size!(InstanceVariableReference, 16);

impl InstanceVariableReference {
    #[must_use]
    pub fn str_id(&self) -> &StringId {
        match self {
            InstanceVariableReference::Unresolved(it) => it.str_id(),
            InstanceVariableReference::Resolved(it) => it.inner.str_id(),
        }
    }

    #[must_use]
    pub fn uri_id(&self) -> UriId {
        match self {
            InstanceVariableReference::Unresolved(it) => it.uri_id(),
            InstanceVariableReference::Resolved(it) => it.inner.uri_id(),
        }
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        match self {
            InstanceVariableReference::Unresolved(it) => it.offset(),
            InstanceVariableReference::Resolved(it) => it.inner.offset(),
        }
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> Option<DefinitionId> {
        match self {
            InstanceVariableReference::Unresolved(it) => it.lexical_nesting_id(),
            InstanceVariableReference::Resolved(it) => it.inner.lexical_nesting_id(),
        }
    }

    #[must_use]
    pub fn id(&self) -> InstanceVariableReferenceId {
        match self {
            InstanceVariableReference::Unresolved(it) => it.id(),
            InstanceVariableReference::Resolved(it) => it.inner.id(),
        }
    }

    #[must_use]
    pub fn declaration_id(&self) -> Option<&DeclarationId> {
        match self {
            InstanceVariableReference::Unresolved(_) => None,
            InstanceVariableReference::Resolved(it) => Some(it.declaration_id()),
        }
    }
}
