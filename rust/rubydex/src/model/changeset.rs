use crate::model::graph::Graph;
use crate::model::identity_maps::{IdentityHashMap, IdentityHashSet};
use crate::model::ids::{DeclarationId, DefinitionId, NameId, ReferenceId, StringId, UriId};
use crate::model::name::Name;

/// Tracks name changes for a single document across updates.
#[derive(Debug, Clone)]
pub struct DocumentChange {
    /// Names the document defined at the time of first update (set once)
    pub old_names: IdentityHashSet<NameId>,
    /// Names the document defines now (updated on each subsequent update)
    pub new_names: IdentityHashSet<NameId>,
}

/// Tracks changes to the index since the last resolution. Used to determine what
/// work needs to be done during incremental resolution.
#[derive(Default, Debug)]
pub struct Changeset {
    pub added_definitions: IdentityHashSet<DefinitionId>,
    /// Tracks per-document name changes since last resolution.
    /// For each document updated, stores the names it had before first update
    /// and the names it has after the latest update.
    pub document_changes: IdentityHashMap<UriId, DocumentChange>,
    /// Declaration IDs that had their ancestor chains invalidated.
    /// These need to be re-linearized during resolution.
    pub invalidated_ancestors: IdentityHashSet<DeclarationId>,
    pub added_constant_references: IdentityHashSet<ReferenceId>,
}

impl Changeset {
    pub(crate) fn record_added_definition(&mut self, id: DefinitionId) {
        self.added_definitions.insert(id);
    }

    pub(crate) fn record_document_change(
        &mut self,
        uri_id: UriId,
        old_names: IdentityHashSet<NameId>,
        new_names: IdentityHashSet<NameId>,
    ) {
        self.document_changes
            .entry(uri_id)
            .and_modify(|change| {
                // Subsequent update: only update new_names
                change.new_names.clone_from(&new_names);
            })
            .or_insert_with(|| DocumentChange {
                // First update: store both old and new
                old_names,
                new_names,
            });
    }

    pub(crate) fn record_invalidated_ancestors(&mut self, ids: impl IntoIterator<Item = DeclarationId>) {
        self.invalidated_ancestors.extend(ids);
    }

    pub(crate) fn record_added_constant_reference(&mut self, id: ReferenceId) {
        self.added_constant_references.insert(id);
    }

    /// Computes names that changed across all document updates.
    /// A name is "changed" if it was added or removed from any document.
    #[must_use]
    pub fn changed_names(&self) -> IdentityHashSet<NameId> {
        let mut changed = IdentityHashSet::default();
        for doc_change in self.document_changes.values() {
            for name_id in doc_change.old_names.symmetric_difference(&doc_change.new_names) {
                changed.insert(*name_id);
            }
        }
        changed
    }

    /// Returns references that may be affected by changes in this changeset.
    #[must_use]
    pub fn affected_references(&self, graph: &Graph) -> IdentityHashSet<ReferenceId> {
        let mut affected = IdentityHashSet::default();
        affected.extend(self.added_constant_references.iter().copied());

        let names = graph.names();
        let searches = graph.searches();

        // Handle changed names (definitions added/removed)
        for name_id in &self.changed_names() {
            let Some(name) = names.get(name_id) else { continue };

            // Direct matches: references with the same name_id
            affected.extend(name.references().iter().copied());

            // Indirect matches: references that searched these namespaces during resolution
            // Only include references whose identifier matches the changed name's identifier
            let identifier = name.str();
            add_references_from_searches(&mut affected, names, searches, *name_id, Some(identifier));
            if let Some(ps_id) = name.parent_scope().as_ref() {
                add_references_from_searches(&mut affected, names, searches, *ps_id, Some(identifier));
            }
            if let Some(n_id) = name.nesting() {
                add_references_from_searches(&mut affected, names, searches, *n_id, Some(identifier));
            }
        }

        // Handle invalidated ancestors - include ALL references that searched those namespaces
        // (can't filter by identifier because ancestor order affects all lookups)
        for decl_id in &self.invalidated_ancestors {
            let Some(decl) = graph.declarations().get(decl_id) else {
                continue;
            };
            for def_id in decl.definitions() {
                let Some(def) = graph.definitions().get(def_id) else {
                    continue;
                };
                let Some(name_id) = def.name_id() else { continue };
                add_references_from_searches(&mut affected, names, searches, *name_id, None);
            }
        }

        affected
    }
}

fn add_references_from_searches(
    affected: &mut IdentityHashSet<ReferenceId>,
    names: &IdentityHashMap<NameId, Name>,
    searches: &IdentityHashMap<NameId, IdentityHashSet<NameId>>,
    namespace_id: NameId,
    identifier: Option<&StringId>,
) {
    let Some(dependent_name_ids) = searches.get(&namespace_id) else {
        return;
    };
    for dependent_name_id in dependent_name_ids {
        let Some(dep_name) = names.get(dependent_name_id) else {
            continue;
        };
        if identifier.is_none_or(|id| dep_name.str() == id) {
            affected.extend(dep_name.references().iter().copied());
        }
    }
}
