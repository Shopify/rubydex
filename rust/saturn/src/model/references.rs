use crate::{
    model::ids::{DeclarationId, UriId},
    offset::Offset,
};

#[derive(Debug)]
pub enum UnresolvedReference {
    Constant(Box<UnresolvedConstantRef>),
}

// An unresolved constant reference is a usage of a constant in a context where we can't immediately determine what it
// refers to. For example:
//
// ```ruby
// module Foo
//   BAR
// end
// ```
//
// Here, we don't immediately know if `BAR` refers to `Foo::BAR` or top level `BAR`. Until we resolve it, it is
// considered an unresolved constant
#[derive(Debug)]
pub struct UnresolvedConstantRef {
    name: String,
    nesting: Vec<DeclarationId>,
    uri_id: UriId,
    offset: Offset,
}

impl UnresolvedConstantRef {
    #[must_use]
    pub fn new(name: String, nesting: Vec<DeclarationId>, uri_id: UriId, offset: Offset) -> Self {
        Self {
            name,
            nesting,
            uri_id,
            offset,
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn nesting(&self) -> &[DeclarationId] {
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
