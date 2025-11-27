use crate::model::{
    identity_maps::IdentityHashMap,
    ids::{DeclarationId, DefinitionId, ReferenceId, StringId},
};

/// A `Declaration` represents the global concept of an entity in Ruby. For example, the class `Foo` may be defined 3
/// times in different files and the `Foo` declaration is the combination of all of those definitions that contribute to
/// the same fully qualified name
#[derive(Debug)]
pub struct Declaration {
    /// The fully qualified name of this declaration
    name: String,
    /// The list of definition IDs that compose this declaration
    definition_ids: Vec<DefinitionId>,
    /// The entities that are owned by this declaration. For example, constants and methods that are defined inside of
    /// the namespace. Note that this is a hashmap of unqualified name IDs to declaration IDs. That assists the
    /// traversal of the graph when trying to resolve constant references or trying to discover which methods exist in a
    /// class
    members: IdentityHashMap<StringId, DeclarationId>,
    /// The list of references that are made to this declaration
    references: Vec<ReferenceId>,
    /// The ID of the owner of this declaration
    owner_id: Option<DeclarationId>,
}

impl Declaration {
    #[must_use]
    pub fn new(name: String) -> Self {
        Self {
            name,
            definition_ids: Vec::new(),
            members: IdentityHashMap::default(),
            references: Vec::new(),
            owner_id: None,
        }
    }

    // Extend this declaration with more definitions by moving `other.definition_ids` inside
    pub fn extend(&mut self, other: Declaration) {
        self.definition_ids.extend(other.definition_ids);
        self.members.extend(other.members);
        self.references.extend(other.references);
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn references(&self) -> &[ReferenceId] {
        &self.references
    }

    #[must_use]
    pub fn definitions(&self) -> &[DefinitionId] {
        &self.definition_ids
    }

    #[must_use]
    pub fn has_no_definitions(&self) -> bool {
        self.definition_ids.is_empty()
    }

    pub fn add_definition(&mut self, definition_id: DefinitionId) {
        debug_assert!(
            !self.definition_ids.contains(&definition_id),
            "Cannot add the same exact definition to a declaration twice. Duplicate definition IDs"
        );

        self.definition_ids.push(definition_id);
    }

    pub fn add_reference(&mut self, id: ReferenceId) {
        self.references.push(id);
    }

    // Deletes a definition from this declaration
    pub fn remove_definition(&mut self, definition_id: &DefinitionId) -> bool {
        if let Some(pos) = self.definition_ids.iter().position(|id| id == definition_id) {
            self.definition_ids.swap_remove(pos);
            self.definition_ids.shrink_to_fit();
            true
        } else {
            false
        }
    }

    #[must_use]
    pub fn members(&self) -> &IdentityHashMap<StringId, DeclarationId> {
        &self.members
    }

    pub fn add_member(&mut self, name_id: StringId, declaration_id: DeclarationId) {
        self.members.insert(name_id, declaration_id);
    }

    pub fn remove_member(&mut self, name_id: &StringId) -> Option<DeclarationId> {
        self.members.remove(name_id)
    }

    #[must_use]
    pub fn get_member(&self, name_id: &StringId) -> Option<&DeclarationId> {
        self.members.get(name_id)
    }

    #[must_use]
    pub fn owner_id(&self) -> &Option<DeclarationId> {
        &self.owner_id
    }

    pub fn set_owner_id(&mut self, owner_id: DeclarationId) {
        self.owner_id = Some(owner_id);
    }

    // This will change once we fix fully qualified names to not use `::` as separators for everything. Also, we may
    // want to actually store this in the struct. Currently, it is only used to cleanup a member that got deleted from
    // the graph, so we're avoiding the extra memory cost by computing it on demand.
    #[must_use]
    pub fn unqualified_name(&self) -> String {
        self.name.rsplit("::").next().unwrap_or(&self.name).to_string()
    }
}

#[cfg(test)]
mod tests {
    // use super::*;

    // #[test]
    // #[should_panic(expected = "Cannot add the same exact definition to a declaration twice. Duplicate definition IDs")]
    // fn inserting_duplicate_definitions() {
    //     let mut decl = Declaration::new("MyDecl".to_string(), DeclarationId::from("Object"));
    //     let def_id = DefinitionId::new(123);

    //     decl.add_definition(def_id);
    //     decl.add_definition(def_id);

    //     assert_eq!(decl.definitions().len(), 1);
    // }

    // #[test]
    // fn adding_and_removing_members() {
    //     let mut decl = Declaration::new("Foo".to_string(), DeclarationId::from("Object"));
    //     let member_name_id = NameId::from("Bar");
    //     let member_decl_id = DeclarationId::from("Foo::Bar");

    //     decl.add_member(member_name_id, member_decl_id);
    //     assert_eq!(decl.members.len(), 1);

    //     let removed = decl.remove_member(&member_name_id);
    //     assert_eq!(removed, Some(member_decl_id));
    //     assert_eq!(decl.members.len(), 0);
    // }

    // #[test]
    // fn unqualified_name() {
    //     let decl = Declaration::new("Foo".to_string(), DeclarationId::from("Foo"));
    //     assert_eq!(decl.unqualified_name(), "Foo");

    //     let decl = Declaration::new("Foo::Bar".to_string(), DeclarationId::from("Foo"));
    //     assert_eq!(decl.unqualified_name(), "Bar");

    //     let decl = Declaration::new("Foo::Bar::baz".to_string(), DeclarationId::from("Foo::Bar"));
    //     assert_eq!(decl.unqualified_name(), "baz");
    // }
}
