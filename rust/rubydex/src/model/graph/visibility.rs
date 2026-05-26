use std::collections::hash_map::Entry;

use super::{Graph, Unit};
use crate::diagnostic::{Diagnostic, Rule};
use crate::model::declaration::Declaration;
use crate::model::definitions::Definition;
use crate::model::identity_maps::IdentityHashSet;
use crate::model::ids::{DeclarationId, DefinitionId, StringId};

#[derive(Default)]
struct VisibilityWork {
    seen: IdentityHashSet<DefinitionId>,
    ids: Vec<DefinitionId>,
}

impl VisibilityWork {
    fn push(&mut self, visibility_definition_id: DefinitionId) {
        if self.seen.insert(visibility_definition_id) {
            self.ids.push(visibility_definition_id);
        }
    }

    fn into_vec(self) -> Vec<DefinitionId> {
        self.ids
    }
}

impl Graph {
    pub(crate) fn add_method_visibility_declaration<F>(
        &mut self,
        definition_id: DefinitionId,
        owner_id: DeclarationId,
        str_id: StringId,
        fully_qualified_name: String,
        constructor: F,
    ) -> DeclarationId
    where
        F: FnOnce(String) -> Declaration,
    {
        let declaration_id =
            self.add_ordered_declaration(definition_id, definition_id, fully_qualified_name, constructor);
        self.applied_method_visibilities.track(owner_id, str_id, definition_id);
        declaration_id
    }

    pub(crate) fn add_module_function_copy_declaration<F>(
        &mut self,
        definition_id: DefinitionId,
        visibility_definition_id: DefinitionId,
        fully_qualified_name: String,
        constructor: F,
    ) -> DeclarationId
    where
        F: FnOnce(String) -> Declaration,
    {
        self.add_ordered_declaration(
            definition_id,
            visibility_definition_id,
            fully_qualified_name,
            constructor,
        )
    }

    fn add_ordered_declaration<F>(
        &mut self,
        definition_id: DefinitionId,
        ordering_visibility_definition_id: DefinitionId,
        fully_qualified_name: String,
        constructor: F,
    ) -> DeclarationId
    where
        F: FnOnce(String) -> Declaration,
    {
        let declaration_id = DeclarationId::from(&fully_qualified_name);
        let insert_index = self.method_visibility_insert_index(declaration_id, ordering_visibility_definition_id);

        match self.declarations.entry(declaration_id) {
            Entry::Occupied(mut occupied_entry) => {
                debug_assert!(
                    occupied_entry.get().name() == fully_qualified_name,
                    "DeclarationId collision in global graph"
                );

                if occupied_entry.get().definitions().contains(&definition_id) {
                    return declaration_id;
                }

                if let Some(index) = insert_index {
                    occupied_entry.get_mut().insert_definition(definition_id, index);
                } else {
                    occupied_entry.get_mut().add_definition(definition_id);
                }
            }
            Entry::Vacant(vacant_entry) => {
                let mut declaration = constructor(fully_qualified_name);
                declaration.add_definition(definition_id);
                vacant_entry.insert(declaration);
            }
        }

        declaration_id
    }

    fn method_visibility_insert_index(
        &self,
        declaration_id: DeclarationId,
        visibility_definition_id: DefinitionId,
    ) -> Option<usize> {
        let visibility_definition = self.definitions.get(&visibility_definition_id)?;
        let Definition::MethodVisibility(_) = visibility_definition else {
            return None;
        };

        let visibility_uri_id = *visibility_definition.uri_id();
        let visibility_uri = self.documents.get(&visibility_uri_id)?.uri();
        let visibility_offset = visibility_definition.offset();

        self.declarations
            .get(&declaration_id)?
            .definitions()
            .iter()
            .position(|definition_id| {
                self.definition_effect_order_key(*definition_id)
                    .is_some_and(|(definition_uri, definition_offset)| {
                        // Insert relative to later definitions in the same file only. Cross-file
                        // URI order is a deterministic resolver approximation, not Ruby load order,
                        // so declaration storage keeps existing cross-document insertion order.
                        definition_uri == visibility_uri && visibility_offset.start() < definition_offset.start()
                    })
            })
    }

    pub(crate) fn add_module_function_copy(
        &mut self,
        source_definition_id: DefinitionId,
        visibility_definition_id: DefinitionId,
        copy_declaration_id: DeclarationId,
        definition: Definition,
    ) -> DefinitionId {
        let definition_id = definition.id();

        if let Some(copy) = self.module_function_copies.get(definition_id) {
            debug_assert_eq!(
                copy.source_definition_id, source_definition_id,
                "Module function copy ID collision with different source definition"
            );
            debug_assert_eq!(
                copy.declaration_id, copy_declaration_id,
                "Module function copy ID collision with different declaration"
            );
            self.attach_module_function_copy_visibility(definition_id, visibility_definition_id);
            return definition_id;
        }

        if self.definitions.contains_key(&definition_id) {
            // `module_function def foo; end` already indexes the public singleton copy as
            // a real document-owned definition. A later `module_function :foo` can compute
            // the same copied method ID. Reuse that definition; its lifetime is still owned
            // by the source document, not by the generated-copy tracker.
            self.module_function_copies
                .attach_document_owned_visibility(source_definition_id, visibility_definition_id);
            return definition_id;
        }

        self.track_definition_strings(&definition);
        self.definitions.insert(definition_id, definition);
        self.module_function_copies.insert(
            definition_id,
            source_definition_id,
            visibility_definition_id,
            copy_declaration_id,
        );

        definition_id
    }

    pub(crate) fn find_module_function_copy(
        &self,
        source_definition_id: DefinitionId,
        copy_declaration_id: DeclarationId,
    ) -> Option<DefinitionId> {
        self.module_function_copies
            .find(source_definition_id, copy_declaration_id)
    }

    pub(crate) fn attach_module_function_copy_visibility(
        &mut self,
        copy_definition_id: DefinitionId,
        visibility_definition_id: DefinitionId,
    ) {
        self.module_function_copies
            .attach_visibility(copy_definition_id, visibility_definition_id);
    }

    pub(crate) fn track_module_function_alias_dependency(
        &mut self,
        alias_definition_id: DefinitionId,
        visibility_definition_id: DefinitionId,
    ) {
        self.module_function_copies
            .track_alias_dependent_visibility(alias_definition_id, visibility_definition_id);
    }

    pub(crate) fn track_module_function_ancestor_dependency(
        &mut self,
        owner_id: DeclarationId,
        str_id: StringId,
        visibility_definition_id: DefinitionId,
    ) {
        self.module_function_copies
            .track_ancestor_dependent_visibility(owner_id, str_id, visibility_definition_id);
    }

    pub(crate) fn track_unresolved_method_visibility(
        &mut self,
        owner_id: DeclarationId,
        str_id: StringId,
        visibility_definition_id: DefinitionId,
    ) {
        if let Some(old_owner_id) =
            self.unresolved_method_visibilities
                .track(owner_id, str_id, visibility_definition_id)
        {
            self.remove_unresolved_method_visibility_diagnostic(old_owner_id, visibility_definition_id);
        }
    }

    pub(crate) fn add_unresolved_method_visibility_diagnostic(
        &mut self,
        owner_id: DeclarationId,
        visibility_definition_id: DefinitionId,
        diagnostic: Diagnostic,
    ) {
        self.remove_unresolved_method_visibility_diagnostic(owner_id, visibility_definition_id);

        let Some(Definition::MethodVisibility(visibility_definition)) = self.definitions.get(&visibility_definition_id)
        else {
            return;
        };

        if visibility_definition.flags().is_singleton_method_visibility() {
            // Document-scoped: the singleton class may be synthetic (created by this
            // visibility resolution) and won't be cleaned up on file delete, so attaching
            // the diagnostic to the declaration would leave it orphaned.
            self.add_document_diagnostic(*visibility_definition.uri_id(), diagnostic);
        } else if let Some(declaration) = self.declarations.get_mut(&owner_id) {
            declaration.add_diagnostic(diagnostic);
        }
    }

    pub(crate) fn mark_method_visibility_resolved(&mut self, visibility_definition_id: DefinitionId) {
        if let Some(owner_id) = self.remove_unresolved_method_visibility(visibility_definition_id) {
            self.remove_unresolved_method_visibility_diagnostic(owner_id, visibility_definition_id);
        }
    }

    pub(crate) fn detach_method_visibility_declaration(&mut self, visibility_definition_id: DefinitionId) {
        let Some(Definition::MethodVisibility(_)) = self.definitions.get(&visibility_definition_id) else {
            return;
        };

        self.detach_applied_method_visibility(visibility_definition_id);

        if let Some(declaration_id) = self.definition_id_to_declaration_id(visibility_definition_id).copied() {
            self.detach_definition_from_declaration(declaration_id, visibility_definition_id);
        }
    }

    pub(super) fn detach_applied_method_visibility(&mut self, visibility_definition_id: DefinitionId) {
        if matches!(
            self.definitions.get(&visibility_definition_id),
            Some(Definition::MethodVisibility(_))
        ) {
            self.applied_method_visibilities.detach(visibility_definition_id);
        }
    }

    pub(super) fn remove_method_visibility_side_effects(
        &mut self,
        visibility_definition_id: DefinitionId,
    ) -> Vec<DefinitionId> {
        let Some(Definition::MethodVisibility(_)) = self.definitions.get(&visibility_definition_id) else {
            return Vec::new();
        };

        self.applied_method_visibilities.detach(visibility_definition_id);
        if let Some(owner_id) = self.remove_unresolved_method_visibility(visibility_definition_id) {
            self.remove_unresolved_method_visibility_diagnostic(owner_id, visibility_definition_id);
        }

        self.remove_module_function_copy_for_visibility(visibility_definition_id)
    }

    pub(super) fn remove_unresolved_method_visibility(
        &mut self,
        visibility_definition_id: DefinitionId,
    ) -> Option<DeclarationId> {
        self.unresolved_method_visibilities.remove(visibility_definition_id)
    }

    pub(crate) fn take_unresolved_method_visibilities_for_owner(
        &mut self,
        owner_id: DeclarationId,
    ) -> Vec<DefinitionId> {
        let visibility_ids = self.unresolved_method_visibilities.take_for_owner(owner_id);

        for visibility_id in &visibility_ids {
            self.remove_unresolved_method_visibility_diagnostic(owner_id, *visibility_id);
        }

        visibility_ids
    }

    pub(crate) fn take_unresolved_method_visibilities_for_owner_and_member(
        &mut self,
        owner_id: DeclarationId,
        member_str_id: StringId,
    ) -> Vec<DefinitionId> {
        let visibility_ids = self
            .unresolved_method_visibilities
            .take_for_owner_and_member(owner_id, member_str_id);

        for visibility_id in &visibility_ids {
            self.remove_unresolved_method_visibility_diagnostic(owner_id, *visibility_id);
        }

        visibility_ids
    }

    pub(crate) fn invalidate_method_visibility_dependents_for_member_change(
        &mut self,
        owner_id: DeclarationId,
        member_str_id: StringId,
    ) -> Vec<DefinitionId> {
        if !self.has_method_visibility_member_change_dependents() {
            return Vec::new();
        }

        let mut visibility_ids = VisibilityWork::default();
        let owner_ids = self.method_visibility_owner_dependents(owner_id);

        self.collect_unresolved_visibility_work_for_member_change(&owner_ids, member_str_id, &mut visibility_ids);
        self.collect_applied_visibility_work_for_member_change(&owner_ids, member_str_id, &mut visibility_ids);
        self.collect_module_function_copy_work_for_member_change(owner_ids, member_str_id, &mut visibility_ids);

        visibility_ids.into_vec()
    }

    fn collect_unresolved_visibility_work_for_member_change(
        &mut self,
        owner_ids: &[DeclarationId],
        member_str_id: StringId,
        visibility_ids: &mut VisibilityWork,
    ) {
        for owner_id in owner_ids {
            let unresolved_visibility_ids =
                self.take_unresolved_method_visibilities_for_owner_and_member(*owner_id, member_str_id);
            self.push_unique_visibility_work(unresolved_visibility_ids, visibility_ids);
        }
    }

    fn collect_applied_visibility_work_for_member_change(
        &mut self,
        owner_ids: &[DeclarationId],
        member_str_id: StringId,
        visibility_ids: &mut VisibilityWork,
    ) {
        for owner_id in owner_ids {
            let member_visibility_ids = self
                .applied_method_visibilities
                .take_for_owner_and_member(*owner_id, member_str_id);
            for visibility_id in member_visibility_ids {
                self.detach_method_visibility_declaration(visibility_id);
                self.push_existing_visibility_work(visibility_id, visibility_ids);
            }
        }
    }

    fn collect_module_function_copy_work_for_member_change(
        &mut self,
        owner_ids: Vec<DeclarationId>,
        member_str_id: StringId,
        visibility_ids: &mut VisibilityWork,
    ) {
        for owner_id in owner_ids {
            let module_function_visibility_ids =
                self.take_module_function_copy_work_for_owner_and_member(owner_id, member_str_id);
            self.push_unique_visibility_work(module_function_visibility_ids, visibility_ids);
        }
    }

    fn has_method_visibility_member_change_dependents(&self) -> bool {
        !self.unresolved_method_visibilities.is_empty()
            || !self.applied_method_visibilities.is_empty()
            || self.module_function_copies.has_ancestor_dependent_visibilities()
    }

    pub(super) fn requeue_unresolved_method_visibilities_for_owner(&mut self, owner_id: DeclarationId) {
        for visibility_id in self.take_unresolved_method_visibilities_for_owner(owner_id) {
            self.push_work(Unit::Definition(visibility_id));
        }
    }

    pub(super) fn remove_unresolved_method_visibility_diagnostic(
        &mut self,
        owner_id: DeclarationId,
        visibility_definition_id: DefinitionId,
    ) {
        let Some(Definition::MethodVisibility(visibility_definition)) = self.definitions.get(&visibility_definition_id)
        else {
            return;
        };
        let uri_id = *visibility_definition.uri_id();
        let offset = visibility_definition.offset().clone();

        if visibility_definition.flags().is_singleton_method_visibility() {
            if let Some(document) = self.documents.get_mut(&uri_id) {
                document.retain_diagnostics(|diagnostic| {
                    diagnostic.rule() != &Rule::UndefinedMethodVisibilityTarget
                        || diagnostic.uri_id() != &uri_id
                        || diagnostic.offset() != &offset
                });
            }
            return;
        }

        if let Some(declaration) = self.declarations.get_mut(&owner_id) {
            declaration.retain_diagnostics(|diagnostic| {
                diagnostic.rule() != &Rule::UndefinedMethodVisibilityTarget
                    || diagnostic.uri_id() != &uri_id
                    || diagnostic.offset() != &offset
            });
        }
    }

    pub(super) fn requeue_method_visibility_definitions_for_declaration(&mut self, declaration_id: DeclarationId) {
        let visibility_ids = self.method_visibility_definition_ids_for_declaration(declaration_id);

        for visibility_id in visibility_ids {
            self.detach_method_visibility_declaration(visibility_id);
            self.push_work(Unit::Definition(visibility_id));
        }
    }

    pub(super) fn requeue_method_visibility_definitions_for_owner(&mut self, owner_id: DeclarationId) {
        let member_ids = self
            .declarations
            .get(&owner_id)
            .and_then(Declaration::as_namespace)
            .map(|namespace| namespace.members().values().copied().collect::<Vec<_>>())
            .unwrap_or_default();

        for member_id in member_ids {
            if matches!(self.declarations.get(&member_id), Some(Declaration::Method(_))) {
                self.requeue_method_visibility_definitions_for_declaration(member_id);
            }
        }
    }

    pub(super) fn requeue_method_visibility_dependents_for_member_change(
        &mut self,
        owner_id: DeclarationId,
        member_str_id: StringId,
    ) {
        for visibility_id in self.invalidate_method_visibility_dependents_for_member_change(owner_id, member_str_id) {
            self.push_work(Unit::Definition(visibility_id));
        }
    }

    fn method_visibility_definition_ids_for_declaration(&self, declaration_id: DeclarationId) -> Vec<DefinitionId> {
        self.declarations
            .get(&declaration_id)
            .map(|declaration| {
                declaration
                    .definitions()
                    .iter()
                    .copied()
                    .filter(|definition_id| {
                        matches!(
                            self.definitions.get(definition_id),
                            Some(Definition::MethodVisibility(_))
                        )
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default()
    }

    fn method_visibility_owner_dependents(&self, owner_id: DeclarationId) -> Vec<DeclarationId> {
        let mut seen_owner_ids = IdentityHashSet::default();
        let mut owner_ids = Vec::new();
        Self::push_unique_owner(owner_id, &mut seen_owner_ids, &mut owner_ids);

        // Descendants are updated whenever ancestor linearization changes. During a method-member change, they are the
        // namespaces whose inherited visibility declarations and generated module_function copies may depend on this
        // owner.
        if let Some(namespace) = self.declarations.get(&owner_id).and_then(Declaration::as_namespace) {
            for descendant_id in namespace.descendants() {
                Self::push_unique_owner(*descendant_id, &mut seen_owner_ids, &mut owner_ids);
            }
        }

        owner_ids
    }

    fn push_unique_owner(
        owner_id: DeclarationId,
        seen_owner_ids: &mut IdentityHashSet<DeclarationId>,
        owner_ids: &mut Vec<DeclarationId>,
    ) {
        if seen_owner_ids.insert(owner_id) {
            owner_ids.push(owner_id);
        }
    }

    fn push_unique_visibility_work(
        &self,
        source_visibility_ids: Vec<DefinitionId>,
        visibility_ids: &mut VisibilityWork,
    ) {
        for visibility_id in source_visibility_ids {
            self.push_existing_visibility_work(visibility_id, visibility_ids);
        }
    }

    pub(super) fn remove_module_function_copies_for_source(
        &mut self,
        source_definition_id: DefinitionId,
    ) -> Vec<DefinitionId> {
        let mut visibility_definition_ids = VisibilityWork::default();
        for visibility_id in self
            .module_function_copies
            .take_document_owned_visibility_ids_for_source(source_definition_id)
        {
            self.push_existing_visibility_work(visibility_id, &mut visibility_definition_ids);
        }
        let copy_ids = self.module_function_copies.copy_ids_for_source(source_definition_id);
        let mut visited_copy_ids = IdentityHashSet::default();

        for copy_id in copy_ids {
            self.remove_module_function_copy_and_collect_visibility_work(
                copy_id,
                &mut visibility_definition_ids,
                &mut visited_copy_ids,
            );
        }

        visibility_definition_ids.into_vec()
    }

    pub(super) fn remove_module_function_copies_for_alias(
        &mut self,
        alias_definition_id: DefinitionId,
    ) -> Vec<DefinitionId> {
        let mut visibility_ids = VisibilityWork::default();
        let alias_visibility_ids = self
            .module_function_copies
            .take_alias_dependent_visibility_ids_for_alias(alias_definition_id);
        for visibility_id in &alias_visibility_ids {
            self.push_existing_visibility_work(*visibility_id, &mut visibility_ids);
        }

        for visibility_id in alias_visibility_ids {
            for dependent_visibility_id in self.remove_module_function_copy_for_visibility(visibility_id) {
                self.push_existing_visibility_work(dependent_visibility_id, &mut visibility_ids);
            }
        }

        visibility_ids.into_vec()
    }

    pub(super) fn remove_module_function_copy_for_visibility(
        &mut self,
        visibility_definition_id: DefinitionId,
    ) -> Vec<DefinitionId> {
        let mut visibility_definition_ids = VisibilityWork::default();
        let affected_copy_definition_ids = self
            .module_function_copies
            .copy_ids_for_visibility(visibility_definition_id);

        self.module_function_copies
            .detach_document_owned_visibility(visibility_definition_id);
        self.module_function_copies
            .detach_alias_dependent_visibility(visibility_definition_id);
        self.module_function_copies
            .detach_ancestor_dependent_visibility(visibility_definition_id);

        let mut visited_copy_ids = IdentityHashSet::default();
        for copy_definition_id in self.module_function_copies.detach_visibility(visibility_definition_id) {
            self.remove_module_function_copy_and_collect_visibility_work(
                copy_definition_id,
                &mut visibility_definition_ids,
                &mut visited_copy_ids,
            );
        }

        let affected_declaration_ids = affected_copy_definition_ids
            .into_iter()
            .filter(|copy_definition_id| self.definitions.contains_key(copy_definition_id))
            .filter_map(|copy_definition_id| self.definition_id_to_declaration_id(copy_definition_id).copied())
            .collect::<Vec<_>>();

        for declaration_id in affected_declaration_ids {
            self.requeue_method_visibility_definitions_for_declaration(declaration_id);
        }

        visibility_definition_ids.into_vec()
    }

    pub(super) fn requeue_module_function_copies_for_owner(&mut self, owner_id: DeclarationId) {
        for visibility_id in self.take_module_function_copy_work_for_owner(owner_id) {
            self.push_work(Unit::Definition(visibility_id));
        }
    }

    fn take_module_function_copy_work_for_owner(&mut self, owner_id: DeclarationId) -> Vec<DefinitionId> {
        let visibility_ids = self
            .module_function_copies
            .take_ancestor_dependent_visibility_ids_for_owner(owner_id);
        self.take_module_function_copy_work_for_visibilities(visibility_ids)
    }

    fn take_module_function_copy_work_for_owner_and_member(
        &mut self,
        owner_id: DeclarationId,
        member_str_id: StringId,
    ) -> Vec<DefinitionId> {
        let visibility_ids = self
            .module_function_copies
            .take_ancestor_dependent_visibility_ids_for_owner_and_member(owner_id, member_str_id);

        self.take_module_function_copy_work_for_visibilities(visibility_ids)
    }

    fn take_module_function_copy_work_for_visibilities(
        &mut self,
        visibility_ids: Vec<DefinitionId>,
    ) -> Vec<DefinitionId> {
        let mut visibility_ids_to_requeue = VisibilityWork::default();

        for visibility_id in visibility_ids {
            self.module_function_copies
                .detach_alias_dependent_visibility(visibility_id);
            let mut visited_copy_ids = IdentityHashSet::default();
            for copy_definition_id in self.module_function_copies.detach_visibility(visibility_id) {
                self.remove_module_function_copy_and_collect_visibility_work(
                    copy_definition_id,
                    &mut visibility_ids_to_requeue,
                    &mut visited_copy_ids,
                );
            }

            self.push_existing_visibility_work(visibility_id, &mut visibility_ids_to_requeue);
        }

        visibility_ids_to_requeue.into_vec()
    }

    fn remove_module_function_copy_and_collect_visibility_work(
        &mut self,
        copy_definition_id: DefinitionId,
        visibility_definition_ids: &mut VisibilityWork,
        visited_copy_ids: &mut IdentityHashSet<DefinitionId>,
    ) {
        if !visited_copy_ids.insert(copy_definition_id) {
            return;
        }

        let dependent_copy_ids = self.module_function_copies.copy_ids_for_source(copy_definition_id);
        for dependent_copy_id in dependent_copy_ids {
            self.remove_module_function_copy_and_collect_visibility_work(
                dependent_copy_id,
                visibility_definition_ids,
                visited_copy_ids,
            );
        }

        for visibility_id in self.remove_module_function_copy(copy_definition_id) {
            self.module_function_copies
                .detach_alias_dependent_visibility(visibility_id);
            self.module_function_copies
                .detach_ancestor_dependent_visibility(visibility_id);
            self.push_existing_visibility_work(visibility_id, visibility_definition_ids);
        }
    }

    fn push_existing_visibility_work(
        &self,
        visibility_definition_id: DefinitionId,
        visibility_definition_ids: &mut VisibilityWork,
    ) {
        if self.definitions.contains_key(&visibility_definition_id) {
            visibility_definition_ids.push(visibility_definition_id);
        }
    }

    fn remove_module_function_copy(&mut self, copy_definition_id: DefinitionId) -> Vec<DefinitionId> {
        let Some(copy) = self.module_function_copies.remove(copy_definition_id) else {
            return Vec::new();
        };

        self.detach_definition_from_declaration(copy.declaration_id, copy_definition_id);

        if let Some(definition) = self.definitions.remove(&copy_definition_id) {
            self.untrack_definition_strings(&definition);
        }

        copy.trigger_definition_ids
    }
}
