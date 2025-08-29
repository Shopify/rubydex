use crate::model::id::Id;
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct NameMarker;
// NameId represents the ID of a fully qualified name, such as `Foo` or `Foo#bar`
pub type NameId = Id<NameMarker>;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct DefinitionMarker;
// DefinitionId represents the ID of a definition found in a specific file
pub type DefinitionId = Id<DefinitionMarker>;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct UriMarker;
// UriId represents the ID of a URI, which is the unique identifier for a document
pub type UriId = Id<UriMarker>;
