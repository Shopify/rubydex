use crate::model::identity_maps::{IdentityHashMap, IdentityHashSet};
use crate::model::ids::{DefinitionId, NameId, ReferenceId};

/// Tracks changes to the index since the last resolution. Used to determine what
/// work needs to be done during incremental resolution.
#[derive(Default, Debug)]
pub struct Changeset {
    pub added_definitions: IdentityHashSet<DefinitionId>,
    pub removed_definitions: IdentityHashSet<DefinitionId>,
    /// Maps definition_id to name_id for definitions that have one.
    /// Used to find affected references after removal (when the definition is gone).
    pub definition_name_ids: IdentityHashMap<DefinitionId, NameId>,
    pub added_constant_references: IdentityHashSet<ReferenceId>,
    pub removed_constant_references: IdentityHashSet<ReferenceId>,
    pub added_method_references: IdentityHashSet<ReferenceId>,
    pub removed_method_references: IdentityHashSet<ReferenceId>,
}

impl Changeset {
    pub(crate) fn record_added_definition(&mut self, id: DefinitionId, name_id: Option<NameId>) {
        if !self.removed_definitions.remove(&id) {
            self.added_definitions.insert(id);
        }
        if let Some(name_id) = name_id {
            self.definition_name_ids.insert(id, name_id);
        }
    }

    pub(crate) fn record_removed_definition(&mut self, id: DefinitionId, name_id: Option<NameId>) {
        if !self.added_definitions.remove(&id) {
            self.removed_definitions.insert(id);
        }
        if let Some(name_id) = name_id {
            self.definition_name_ids.insert(id, name_id);
        }
    }

    pub(crate) fn record_added_constant_reference(&mut self, id: ReferenceId) {
        if !self.removed_constant_references.remove(&id) {
            self.added_constant_references.insert(id);
        }
    }

    pub(crate) fn record_removed_constant_reference(&mut self, id: ReferenceId) {
        if !self.added_constant_references.remove(&id) {
            self.removed_constant_references.insert(id);
        }
    }

    pub(crate) fn record_added_method_reference(&mut self, id: ReferenceId) {
        if !self.removed_method_references.remove(&id) {
            self.added_method_references.insert(id);
        }
    }

    pub(crate) fn record_removed_method_reference(&mut self, id: ReferenceId) {
        if !self.added_method_references.remove(&id) {
            self.removed_method_references.insert(id);
        }
    }
}
