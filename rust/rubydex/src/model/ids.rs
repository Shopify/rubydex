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
pub struct StringMarker;
/// `StringId` represents an ID for an interned string value
pub type StringId = Id<StringMarker>;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct ReferenceMarker;
/// `ReferenceId` represents the ID of a reference occurrence in a file.
/// It is built from the reference kind, `uri_id` and the reference `offset`.
pub type ReferenceId = Id<ReferenceMarker>;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct NameMarker;
/// `NameId` represents an ID for any constant name that we find as part of a reference or definition
pub type NameId = Id<NameMarker>;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct DiagnosticMarker;
/// `DiagnosticId` represents the ID of a diagnostic found in a specific file
pub type DiagnosticId = Id<DiagnosticMarker>;
