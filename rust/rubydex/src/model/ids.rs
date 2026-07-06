use crate::{
    assert_mem_size,
    model::{definitions::Receiver, id::Id},
    offset::Offset,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct DeclarationMarker;
/// `DeclarationId` represents the ID of a fully qualified name. For example, `Foo::Bar` or `Foo#my_method`
pub type DeclarationId = Id<DeclarationMarker>;

/// Creates a `DeclarationId` from a user-facing lookup name.
///
/// Declaration names are stored without the root-scope marker, so `"::Object"`
/// and `"Object"` must resolve to the same declaration ID.
#[must_use]
pub fn declaration_id_from_lookup_name(name: &str) -> DeclarationId {
    DeclarationId::from(name.strip_prefix("::").unwrap_or(name))
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct DefinitionMarker;

// DefinitionId represents the ID of a definition found in a specific file
pub type DefinitionId = Id<DefinitionMarker>;
assert_mem_size!(DefinitionId, 8);

#[must_use]
pub fn namespace_definition_id(uri_id: UriId, offset: &Offset, name_id: NameId) -> DefinitionId {
    DefinitionId::from(&format!("{}{}{}", *uri_id, offset.start(), *name_id))
}

#[must_use]
pub fn method_definition_id(
    uri_id: UriId,
    offset: &Offset,
    str_id: StringId,
    receiver: Option<&Receiver>,
) -> DefinitionId {
    let mut formatted_id = format!("{}{}{}", *uri_id, offset.start(), *str_id);
    if let Some(receiver) = receiver {
        match receiver {
            Receiver::SelfReceiver(def_id) => formatted_id.push_str(&def_id.to_string()),
            Receiver::ConstantReceiver(name_id) => formatted_id.push_str(&name_id.to_string()),
        }
    }
    DefinitionId::from(&formatted_id)
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct UriMarker;
// UriId represents the ID of a URI, which is the unique identifier for a document
pub type UriId = Id<UriMarker>;
assert_mem_size!(UriId, 8);

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct StringMarker;
/// `StringId` represents an ID for an interned string value
pub type StringId = Id<StringMarker>;
assert_mem_size!(StringId, 8);

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct NameMarker;
/// `NameId` represents an ID for any constant name that we find as part of a reference or definition
pub type NameId = Id<NameMarker>;
assert_mem_size!(NameId, 8);

// Reference IDs
//
// This section is for specialized IDs for each type of declaration reference

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct ConstantMarker;
pub type ConstantReferenceId = Id<ConstantMarker>;
assert_mem_size!(ConstantReferenceId, 8);

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct MethodMarker;
pub type MethodReferenceId = Id<MethodMarker>;
assert_mem_size!(MethodReferenceId, 8);

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct GlobalVariableMarker;
pub type GlobalVariableReferenceId = Id<GlobalVariableMarker>;
assert_mem_size!(GlobalVariableReferenceId, 8);

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct ClassVariableMarker;
pub type ClassVariableReferenceId = Id<ClassVariableMarker>;
assert_mem_size!(ClassVariableReferenceId, 8);

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct InstanceVariableMarker;
pub type InstanceVariableReferenceId = Id<InstanceVariableMarker>;
assert_mem_size!(InstanceVariableReferenceId, 8);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn declaration_id_from_lookup_name_accepts_root_scope_marker() {
        assert_eq!(
            DeclarationId::from("Object"),
            declaration_id_from_lookup_name("::Object")
        );
        assert_eq!(
            DeclarationId::from("Foo::Bar"),
            declaration_id_from_lookup_name("::Foo::Bar")
        );
        assert_ne!(
            DeclarationId::from("Object"),
            declaration_id_from_lookup_name("::::Object")
        );
    }
}
