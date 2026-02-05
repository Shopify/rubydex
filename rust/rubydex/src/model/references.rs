use crate::{
    assert_mem_size,
    model::ids::{NameId, ReferenceId, StringId, UriId},
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
    pub fn id(&self) -> ReferenceId {
        ReferenceId::from(&format!(
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
assert_mem_size!(MethodRef, 40);

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
    pub fn id(&self) -> ReferenceId {
        ReferenceId::from(&format!(
            "{}:{}:{}-{}",
            self.str,
            self.uri_id,
            self.offset.start(),
            self.offset.end()
        ))
    }
}
