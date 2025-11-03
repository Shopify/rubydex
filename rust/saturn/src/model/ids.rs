use crate::model::id::Id;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct DeclarationMarker;
/// `DeclarationId` represents the ID of a fully qualified name. For example, `Foo::Bar` or `Foo#my_method`
pub type DeclarationId = Id<DeclarationMarker>;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct DefinitionMarker;
// DefinitionId represents the ID of a definition found in a specific file
pub type DefinitionId = Id<DefinitionMarker>;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct UriMarker;
// UriId represents the ID of a URI, which is the unique identifier for a document
pub type UriId = Id<UriMarker>;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
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

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct ReferenceMarker;
/// `ReferenceId` represents the ID of a reference occurrence in a file.
/// It is built from the reference kind, `uri_id` and the reference `offset`.
pub type ReferenceId = Id<ReferenceMarker>;
