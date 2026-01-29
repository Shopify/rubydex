use crate::model::identity_maps::{IdentityHashMap, IdentityHashSet};
use crate::model::ids::{DeclarationId, DefinitionId, NameId, ReferenceId};
use crate::model::name::Name;

/// Tracks changes to the index since the last resolution. Used to determine what
/// work needs to be done during incremental resolution.
#[derive(Default, Debug)]
pub struct Changeset {
    pub added_definitions: IdentityHashSet<DefinitionId>,
    pub removed_definitions: IdentityHashSet<DefinitionId>,
    /// Maps definition_id to name_id for definitions that have one.
    /// Used to find affected references after removal (when the definition is gone).
    pub definition_name_ids: IdentityHashMap<DefinitionId, NameId>,
    /// Declaration IDs that had their ancestor chains invalidated.
    /// These need to be re-linearized during resolution.
    pub invalidated_ancestors: IdentityHashSet<DeclarationId>,
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

    pub(crate) fn record_invalidated_ancestors(&mut self, ids: impl IntoIterator<Item = DeclarationId>) {
        self.invalidated_ancestors.extend(ids);
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

    /// Returns references that may be affected by changes in this changeset.
    #[must_use]
    pub fn affected_references(
        &self,
        names: &IdentityHashMap<NameId, Name>,
        searches: &IdentityHashMap<NameId, IdentityHashSet<NameId>>,
    ) -> IdentityHashSet<ReferenceId> {
        let mut affected = IdentityHashSet::default();
        affected.extend(self.added_constant_references.iter().copied());

        for definition_id in self.added_definitions.iter().chain(self.removed_definitions.iter()) {
            let Some(name_id) = self.definition_name_ids.get(definition_id) else {
                continue;
            };

            let name = names.get(name_id);

            // Direct matches: references with the same name_id
            if let Some(name) = name {
                for reference_id in name.references() {
                    affected.insert(*reference_id);
                }
            }

            // Indirect matches: references that searched these namespaces during resolution
            let mut namespace_ids = vec![*name_id];
            if let Some(name) = name {
                if let Some(ps_id) = name.parent_scope().as_ref() {
                    namespace_ids.push(*ps_id);
                }
                if let Some(n_id) = name.nesting() {
                    namespace_ids.push(*n_id);
                }
            }

            for ns_id in namespace_ids {
                if let Some(dependent_name_ids) = searches.get(&ns_id) {
                    for dependent_name_id in dependent_name_ids {
                        if let Some(dep_name) = names.get(dependent_name_id) {
                            for reference_id in dep_name.references() {
                                affected.insert(*reference_id);
                            }
                        }
                    }
                }
            }
        }

        affected
    }
}
