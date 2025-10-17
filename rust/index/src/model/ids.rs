use crate::model::id::Id;
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct DeclarationMarker;
/// `DeclarationId` represents the ID of a fully qualified name. For example, `Foo::Bar` or `Foo#my_method`
pub type DeclarationId = Id<DeclarationMarker>;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct DefinitionMarker;
// DefinitionId represents the ID of a definition found in a specific file
pub type DefinitionId = Id<DefinitionMarker>;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct UriMarker;
// UriId represents the ID of a URI, which is the unique identifier for a document
pub type UriId = Id<UriMarker>;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct NameMarker;
/// `NameId` represents the ID of a non qualified name reference. For example
///
/// ```ruby
/// module Foo
///   class Bar
///   end
/// end
/// ```
///
/// In here, the `NameId` for `Bar` is just the hashed `Bar` string, not `Foo::Bar`
pub type NameId = Id<NameMarker>;
