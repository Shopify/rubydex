use crate::{
    model::ids::{NameId, UriId},
    offset::Offset,
};

// A constant reference tracks where constants are used in the codebase, including the name,
// the nesting context where it was found, and its location in the source file.
#[derive(Debug)]
pub struct ConstantRef {
    name: String,
    nesting: Vec<NameId>,
    uri_id: UriId,
    offset: Offset,
}

impl ConstantRef {
    #[must_use]
    pub fn new(name: String, nesting: Vec<NameId>, uri_id: UriId, offset: Offset) -> Self {
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

// An unresolved constant reference is a usage of a constant in a context where we can't immediately
// determine what it refers to. For example:
//
// ```ruby
// module Foo
//   BAR
// end
// ```
//
// Here, we don't immediately know if `BAR` refers to `Foo::BAR` or top-level `BAR`. These references
// need to be resolved later by walking up the nesting hierarchy and checking what constants exist.
#[derive(Debug)]
pub enum UnresolvedReference {
    Constant(Box<ConstantRef>),
}

// A resolved constant reference is a usage of a constant that we can immediately determine what it
// refers to.
// Currently, this only supports top-level constants prefixed with `::`. For example:
//
// ```ruby
// class Foo
//   ::Object      # Resolved - definitely refers to top-level Object
//   ::Foo::Bar    # Resolved - definitely refers to top-level Foo::Bar
// end
// ```
//
// The `::` prefix makes it unambiguous that these refer to top-level constants, not relative ones.
#[derive(Debug)]
pub enum ResolvedReference {
    Constant(Box<ConstantRef>),
}
