use crate::{
    indexing::scope::Nesting,
    model::ids::{DeclarationId, NameId, ReferenceId, UriId},
    offset::Offset,
};
use std::sync::Arc;

/// An unresolved reference to a constant
#[derive(Debug)]
pub struct UnresolvedConstantRef {
    /// The unqualified name of the constant
    name_id: NameId,
    /// The nesting where we found the constant reference. This is a chain of nesting objects representing the lexical
    /// scopes surrounding the reference
    nesting: Option<Arc<Nesting>>,
    /// The document where we found the reference
    uri_id: UriId,
    /// The offsets inside of the document where we found the reference
    offset: Offset,
}

impl UnresolvedConstantRef {
    #[must_use]
    pub fn new(name_id: NameId, nesting: Option<Arc<Nesting>>, uri_id: UriId, offset: Offset) -> Self {
        Self {
            name_id,
            nesting,
            uri_id,
            offset,
        }
    }

    #[must_use]
    pub fn name_id(&self) -> &NameId {
        &self.name_id
    }

    #[must_use]
    pub fn nesting(&self) -> &Option<Arc<Nesting>> {
        &self.nesting
    }

    #[must_use]
    pub fn uri_id(&self) -> UriId {
        self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }
}

/// A resolved reference to a constant
#[derive(Debug)]
pub struct ConstantRef {
    /// The original unresolved constant reference data. This is preserved so that we can figure out when this reference
    /// gets invalidated due to a new definition or an inheritance change
    original_ref: UnresolvedConstantRef,
    declaration_id: DeclarationId,
}

impl ConstantRef {
    #[must_use]
    pub fn new(original_ref: UnresolvedConstantRef, declaration_id: DeclarationId) -> Self {
        Self {
            original_ref,
            declaration_id,
        }
    }

    #[must_use]
    pub fn name_id(&self) -> &NameId {
        &self.original_ref.name_id
    }

    #[must_use]
    pub fn nesting(&self) -> &Option<Arc<Nesting>> {
        &self.original_ref.nesting
    }

    #[must_use]
    pub fn uri_id(&self) -> UriId {
        self.original_ref.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.original_ref.offset
    }

    #[must_use]
    pub fn declaration_id(&self) -> DeclarationId {
        self.declaration_id
    }
}

/// An unresolved or resolved constant reference
#[derive(Debug)]
pub enum ConstantReference {
    Resolved(Box<ConstantRef>),
    Unresolved(Box<UnresolvedConstantRef>),
}

impl ConstantReference {
    #[must_use]
    pub fn id(&self) -> ReferenceId {
        // C:<uri_id>:<start>-<end>
        match self {
            ConstantReference::Unresolved(constant) => {
                let key = format!(
                    "C:{}:{}:{}-{}",
                    constant.name_id(),
                    constant.uri_id(),
                    constant.offset().start(),
                    constant.offset().end()
                );
                ReferenceId::from(&key)
            }
            ConstantReference::Resolved(constant) => {
                let key = format!(
                    "C:{}:{}:{}-{}",
                    constant.name_id(),
                    constant.uri_id(),
                    constant.offset().start(),
                    constant.offset().end()
                );
                ReferenceId::from(&key)
            }
        }
    }

    #[must_use]
    pub fn name_id(&self) -> &NameId {
        match self {
            ConstantReference::Unresolved(constant) => constant.name_id(),
            ConstantReference::Resolved(constant) => constant.name_id(),
        }
    }
}

/// A reference to a method
#[derive(Debug)]
pub struct MethodRef {
    /// The unqualified name of the method
    name_id: NameId,
    /// The document where we found the reference
    uri_id: UriId,
    /// The offsets inside of the document where we found the reference
    offset: Offset,
}

impl MethodRef {
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
    pub fn id(&self) -> ReferenceId {
        // M:<uri_id>:<start>-<end>
        let key = format!(
            "M:{}:{}:{}-{}",
            self.name_id,
            self.uri_id,
            self.offset.start(),
            self.offset.end()
        );
        ReferenceId::from(&key)
    }
}
