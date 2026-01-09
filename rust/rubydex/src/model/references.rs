use crate::{
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
        // C:<uri_id>:<start>-<end>
        let key = format!(
            "C:{}:{}:{}-{}",
            self.name_id,
            self.uri_id,
            self.offset.start(),
            self.offset.end()
        );
        ReferenceId::from(&key)
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
}

impl MethodRef {
    #[must_use]
    pub fn new(str: StringId, uri_id: UriId, offset: Offset) -> Self {
        Self { str, uri_id, offset }
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
    pub fn id(&self) -> ReferenceId {
        // M:<uri_id>:<start>-<end>
        let key = format!(
            "M:{}:{}:{}-{}",
            self.str,
            self.uri_id,
            self.offset.start(),
            self.offset.end()
        );
        ReferenceId::from(&key)
    }
}
