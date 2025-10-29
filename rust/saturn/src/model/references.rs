use crate::{
    indexing::scope::Nesting,
    model::ids::{NameId, ReferenceId, UriId},
    offset::Offset,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

#[derive(Debug, Serialize, Deserialize)]
pub struct ConstantReference {
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

#[derive(Debug, Serialize, Deserialize)]
pub struct MethodReference {
    /// The unqualified name of the method
    name_id: NameId,
    /// The document where we found the reference
    uri_id: UriId,
    /// The offsets inside of the document where we found the reference
    offset: Offset,
}

#[derive(Debug, Serialize, Deserialize)]
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
    Constant(Box<ConstantReference>),

    /// An unresolved method reference is a usage of a method in a context where we can't immediately determine what it
    /// refers to. For example:
    ///
    /// ```ruby
    /// module Foo
    ///   bar
    /// end
    /// ```
    ///
    /// Here, we don't immediately know if which `bar` method the call refers to. Until we resolve it, it is
    /// considered an unresolved method. For this example, `name_id` would be `NameId::from("bar")`.
    Method(Box<MethodReference>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ResolvedReference {
    Constant(Box<ConstantReference>),
    Method(Box<MethodReference>),
}

impl UnresolvedReference {
    #[must_use]
    pub fn id(&self) -> ReferenceId {
        match self {
            UnresolvedReference::Constant(constant) => {
                // C:<uri_id>:<start>-<end>
                let key = format!(
                    "C:{}:{}:{}-{}",
                    constant.name_id(),
                    constant.uri_id(),
                    constant.offset().start(),
                    constant.offset().end()
                );
                ReferenceId::from(&key)
            }
            UnresolvedReference::Method(method) => {
                // M:<uri_id>:<start>-<end>
                let key = format!(
                    "M:{}:{}:{}-{}",
                    method.name_id(),
                    method.uri_id(),
                    method.offset().start(),
                    method.offset().end()
                );
                ReferenceId::from(&key)
            }
        }
    }
}

impl ConstantReference {
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

impl MethodReference {
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
}
