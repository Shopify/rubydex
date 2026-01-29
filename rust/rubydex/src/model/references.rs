use crate::{
    assert_mem_size,
    model::ids::{NameId, ReferenceId, StringId, UriId},
    offset::Offset,
};

#[repr(u8)]
#[derive(PartialEq, Debug)]
pub enum ReferenceKind {
    Constant = 0,
    Method = 1,
}

impl From<u8> for ReferenceKind {
    fn from(value: u8) -> Self {
        match value {
            0 => ReferenceKind::Constant,
            1 => ReferenceKind::Method,
            _ => panic!("Invalid ReferenceKind value: {value}"),
        }
    }
}

impl From<ReferenceKind> for u8 {
    fn from(kind: ReferenceKind) -> Self {
        kind as u8
    }
}

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
assert_mem_size!(ConstantReference, 16);

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
        let mut id = ReferenceId::from(&format!(
            "{}:{}:{}-{}",
            self.name_id,
            self.uri_id,
            self.offset.start(),
            self.offset.end()
        ));
        id.tag_kind(ReferenceKind::Constant);
        id
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
assert_mem_size!(MethodRef, 24);

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
        let mut id = ReferenceId::from(&format!(
            "{}:{}:{}-{}",
            self.str,
            self.uri_id,
            self.offset.start(),
            self.offset.end()
        ));
        id.tag_kind(ReferenceKind::Method);
        id
    }
}
