use crate::{
    model::ids::{NameId, UriId},
    offset::Offset,
};

#[derive(Debug)]
pub struct ConstantRef {
    /// The unqualified name of the constant
    name_id: NameId,
    /// The nesting where we found the constant reference. This is a list of unqualified name IDs, so that we can
    /// traverse the graph through the member relationships
    nesting: Vec<NameId>,
    /// The document where we found the reference
    uri_id: UriId,
    /// The offsets inside of the document where we found the reference
    offset: Offset,
}

#[derive(Debug)]
pub enum UnresolvedReference {
    /// An unresolved constant reference is a usage of a constant in a context where we can't immediately determine what it
    /// refers to. For example:
    ///
    /// ```ruby
    /// module Foo
    ///   BAR
    /// end
    /// ```
    ///
    /// Here, we don't immediately know if `BAR` refers to `Foo::BAR` or top level `BAR`. Until we resolve it, it is
    /// considered an unresolved constant. For this example, `name_id` would be `NameId::from("BAR")` and `nesting` would be
    /// `[NameId::from("Foo")]`.
    Constant(Box<ConstantRef>),
}

#[derive(Debug)]
pub enum ResolvedReference {
    Constant(Box<ConstantRef>),
}

impl ConstantRef {
    #[must_use]
    pub fn new(name_id: NameId, nesting: Vec<NameId>, uri_id: UriId, offset: Offset) -> Self {
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
    pub fn nesting(&self) -> &[NameId] {
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
