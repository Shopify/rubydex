use std::{
    collections::hash_map::Entry,
    hash::{Hash, Hasher},
};

#[cfg(test)]
use crate::model::definitions::Definition;
use crate::model::identity_maps::IdentityHashMap;
use crate::model::ids::{DeclarationId, DefinitionId, StringId};

fn push_unique<T: Copy + Eq>(values: &mut Vec<T>, value: T) {
    if !values.contains(&value) {
        values.push(value);
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct MethodVisibilityDependencyKey {
    owner_id: DeclarationId,
    str_id: StringId,
}

impl MethodVisibilityDependencyKey {
    fn new(owner_id: DeclarationId, str_id: StringId) -> Self {
        Self { owner_id, str_id }
    }
}

impl Hash for MethodVisibilityDependencyKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let owner = self.owner_id.get();
        let name = self.str_id.get();
        state.write_u64(owner.rotate_left(17) ^ name.rotate_right(11));
    }
}

/// Small bidirectional multi-map for feature-local reverse indexes.
///
/// Values are stored in `Vec`s because the expected fanout is tiny: a method visibility usually depends on one alias,
/// one owner, or one source method. If these relationships become high-cardinality, switch the lists to sets.
///
/// This uses `IdentityHashMap`, so keys and values must hash through a single `write_u64`/`write_u32` path. The current
/// users are graph IDs and `MethodVisibilityDependencyKey`, which implements that explicitly.
#[derive(Debug)]
struct BiMultiMap<K, V> {
    forward: IdentityHashMap<K, Vec<V>>,
    reverse: IdentityHashMap<V, Vec<K>>,
}

impl<K, V> Default for BiMultiMap<K, V> {
    fn default() -> Self {
        Self {
            forward: IdentityHashMap::default(),
            reverse: IdentityHashMap::default(),
        }
    }
}

impl<K, V> BiMultiMap<K, V>
where
    K: Copy + Eq + Hash,
    V: Copy + Eq + Hash,
{
    fn insert(&mut self, key: K, value: V) {
        push_unique(self.forward.entry(key).or_default(), value);
        push_unique(self.reverse.entry(value).or_default(), key);
    }

    fn remove_key(&mut self, key: K) -> Vec<V> {
        let values = self.forward.remove(&key).unwrap_or_default();

        for value in &values {
            if let Some(keys) = self.reverse.get_mut(value) {
                keys.retain(|id| *id != key);
                if keys.is_empty() {
                    self.reverse.remove(value);
                }
            }
        }

        values
    }

    fn remove_value(&mut self, value: V) -> Vec<K> {
        let keys = self.reverse.remove(&value).unwrap_or_default();

        for key in &keys {
            if let Some(values) = self.forward.get_mut(key) {
                values.retain(|id| *id != value);
                if values.is_empty() {
                    self.forward.remove(key);
                }
            }
        }

        keys
    }

    fn is_empty(&self) -> bool {
        self.forward.is_empty()
    }

    fn debug_assert_consistent(&self) {
        #[cfg(debug_assertions)]
        {
            for (key, values) in &self.forward {
                for value in values {
                    debug_assert!(
                        self.reverse.get(value).is_some_and(|keys| keys.contains(key)),
                        "BiMultiMap missing reverse edge"
                    );
                }
            }

            for (value, keys) in &self.reverse {
                for key in keys {
                    debug_assert!(
                        self.forward.get(key).is_some_and(|values| values.contains(value)),
                        "BiMultiMap missing forward edge"
                    );
                }
            }
        }
    }

    #[cfg(test)]
    fn values(&self) -> impl Iterator<Item = V> + '_ {
        self.reverse.keys().copied()
    }
}

#[derive(Debug)]
pub(crate) struct GeneratedMethodCopy {
    pub(crate) source_definition_id: DefinitionId,
    pub(crate) trigger_definition_ids: Vec<DefinitionId>,
    pub(crate) declaration_id: DeclarationId,
}

/// Generated method copies keyed by generated definition id, source definition id, and trigger definition id.
///
/// This is generic enough for copied-method lifecycle tracking, but currently only `module_function :name` uses it.
#[derive(Default, Debug)]
struct GeneratedMethodCopies {
    definitions: IdentityHashMap<DefinitionId, GeneratedMethodCopy>,
    by_source: IdentityHashMap<DefinitionId, Vec<DefinitionId>>,
    by_trigger: IdentityHashMap<DefinitionId, Vec<DefinitionId>>,
}

impl GeneratedMethodCopies {
    fn get(&self, generated_definition_id: DefinitionId) -> Option<&GeneratedMethodCopy> {
        self.definitions.get(&generated_definition_id)
    }

    fn find(&self, source_definition_id: DefinitionId, declaration_id: DeclarationId) -> Option<DefinitionId> {
        self.by_source
            .get(&source_definition_id)?
            .iter()
            .copied()
            .find(|generated_definition_id| {
                self.definitions
                    .get(generated_definition_id)
                    .is_some_and(|definition| definition.declaration_id == declaration_id)
            })
    }

    fn insert(
        &mut self,
        generated_definition_id: DefinitionId,
        source_definition_id: DefinitionId,
        trigger_definition_id: DefinitionId,
        declaration_id: DeclarationId,
    ) {
        match self.definitions.entry(generated_definition_id) {
            Entry::Occupied(_) => panic!("generated definition inserted twice"),
            Entry::Vacant(entry) => {
                entry.insert(GeneratedMethodCopy {
                    source_definition_id,
                    trigger_definition_ids: Vec::new(),
                    declaration_id,
                });
            }
        }

        let generated_ids = self.by_source.entry(source_definition_id).or_default();
        push_unique(generated_ids, generated_definition_id);

        self.attach_trigger(generated_definition_id, trigger_definition_id);
    }

    fn attach_trigger(&mut self, generated_definition_id: DefinitionId, trigger_definition_id: DefinitionId) {
        let definition = self
            .definitions
            .get_mut(&generated_definition_id)
            .expect("generated definition should exist before attaching trigger");

        push_unique(&mut definition.trigger_definition_ids, trigger_definition_id);

        let generated_ids = self.by_trigger.entry(trigger_definition_id).or_default();
        push_unique(generated_ids, generated_definition_id);
    }

    fn ids_for_source(&self, source_definition_id: DefinitionId) -> Vec<DefinitionId> {
        self.by_source.get(&source_definition_id).cloned().unwrap_or_default()
    }

    fn ids_for_trigger(&self, trigger_definition_id: DefinitionId) -> Vec<DefinitionId> {
        self.by_trigger.get(&trigger_definition_id).cloned().unwrap_or_default()
    }

    fn detach_trigger(&mut self, trigger_definition_id: DefinitionId) -> Vec<DefinitionId> {
        let Some(generated_definition_ids) = self.by_trigger.remove(&trigger_definition_id) else {
            return Vec::new();
        };

        generated_definition_ids
            .into_iter()
            .filter(|generated_definition_id| {
                if let Some(definition) = self.definitions.get_mut(generated_definition_id) {
                    definition
                        .trigger_definition_ids
                        .retain(|id| *id != trigger_definition_id);
                    definition.trigger_definition_ids.is_empty()
                } else {
                    false
                }
            })
            .collect()
    }

    fn remove(&mut self, generated_definition_id: DefinitionId) -> Option<GeneratedMethodCopy> {
        let definition = self.definitions.remove(&generated_definition_id)?;

        for trigger_id in &definition.trigger_definition_ids {
            if let Some(generated_ids) = self.by_trigger.get_mut(trigger_id) {
                generated_ids.retain(|id| *id != generated_definition_id);
                if generated_ids.is_empty() {
                    self.by_trigger.remove(trigger_id);
                }
            }
        }

        let remove_source_entry = if let Some(generated_ids) = self.by_source.get_mut(&definition.source_definition_id)
        {
            generated_ids.retain(|id| *id != generated_definition_id);
            generated_ids.is_empty()
        } else {
            false
        };
        if remove_source_entry {
            self.by_source.remove(&definition.source_definition_id);
        }

        Some(definition)
    }

    fn debug_assert_consistent(&self) {
        #[cfg(debug_assertions)]
        {
            for (generated_id, definition) in &self.definitions {
                debug_assert!(
                    self.by_source
                        .get(&definition.source_definition_id)
                        .is_some_and(|ids| ids.contains(generated_id)),
                    "generated definition missing source reverse edge"
                );

                for trigger_id in &definition.trigger_definition_ids {
                    debug_assert!(
                        self.by_trigger
                            .get(trigger_id)
                            .is_some_and(|ids| ids.contains(generated_id)),
                        "generated definition missing trigger reverse edge"
                    );
                }
            }

            for (source_id, generated_ids) in &self.by_source {
                for (idx, generated_id) in generated_ids.iter().enumerate() {
                    debug_assert!(
                        !generated_ids[..idx].contains(generated_id),
                        "source reverse edge contains duplicate generated definition"
                    );
                    debug_assert!(
                        self.definitions
                            .get(generated_id)
                            .is_some_and(|definition| definition.source_definition_id == *source_id),
                        "source reverse edge points to missing or mismatched generated definition"
                    );
                }
            }

            for (trigger_id, generated_ids) in &self.by_trigger {
                for (idx, generated_id) in generated_ids.iter().enumerate() {
                    debug_assert!(
                        !generated_ids[..idx].contains(generated_id),
                        "trigger reverse edge contains duplicate generated definition"
                    );
                    debug_assert!(
                        self.definitions
                            .get(generated_id)
                            .is_some_and(|definition| definition.trigger_definition_ids.contains(trigger_id)),
                        "trigger reverse edge points to missing or mismatched generated definition"
                    );
                }
            }
        }
    }
}

/// Reverse indexes for generated methods created by retroactive `module_function :name`.
///
/// The generated definition's source location belongs to the copied source method, while its lifetime depends on both
/// the source definition and the visibility definition that requested the copy.
#[derive(Default, Debug)]
pub(crate) struct ModuleFunctionCopies {
    generated: GeneratedMethodCopies,
    alias_dependent_visibilities: BiMultiMap<DefinitionId, DefinitionId>,
    ancestor_dependent_visibilities: BiMultiMap<DeclarationId, DefinitionId>,
    ancestor_dependent_visibilities_by_member: BiMultiMap<MethodVisibilityDependencyKey, DefinitionId>,
    document_owned_visibilities: BiMultiMap<DefinitionId, DefinitionId>,
}

impl ModuleFunctionCopies {
    pub(crate) fn get(&self, copy_definition_id: DefinitionId) -> Option<&GeneratedMethodCopy> {
        self.generated.get(copy_definition_id)
    }

    pub(crate) fn find(
        &self,
        source_definition_id: DefinitionId,
        copy_declaration_id: DeclarationId,
    ) -> Option<DefinitionId> {
        self.generated.find(source_definition_id, copy_declaration_id)
    }

    pub(crate) fn insert(
        &mut self,
        copy_definition_id: DefinitionId,
        source_definition_id: DefinitionId,
        visibility_definition_id: DefinitionId,
        copy_declaration_id: DeclarationId,
    ) {
        self.generated.insert(
            copy_definition_id,
            source_definition_id,
            visibility_definition_id,
            copy_declaration_id,
        );
    }

    pub(crate) fn attach_visibility(
        &mut self,
        copy_definition_id: DefinitionId,
        visibility_definition_id: DefinitionId,
    ) {
        self.generated
            .attach_trigger(copy_definition_id, visibility_definition_id);
    }

    pub(crate) fn track_alias_dependent_visibility(
        &mut self,
        alias_definition_id: DefinitionId,
        visibility_definition_id: DefinitionId,
    ) {
        self.alias_dependent_visibilities
            .insert(alias_definition_id, visibility_definition_id);
        self.debug_assert_consistent();
    }

    pub(crate) fn take_alias_dependent_visibility_ids_for_alias(
        &mut self,
        alias_definition_id: DefinitionId,
    ) -> Vec<DefinitionId> {
        let visibility_ids = self.alias_dependent_visibilities.remove_key(alias_definition_id);
        self.debug_assert_consistent();
        visibility_ids
    }

    pub(crate) fn detach_alias_dependent_visibility(&mut self, visibility_definition_id: DefinitionId) {
        self.alias_dependent_visibilities.remove_value(visibility_definition_id);
        self.debug_assert_consistent();
    }

    pub(crate) fn track_ancestor_dependent_visibility(
        &mut self,
        owner_id: DeclarationId,
        str_id: StringId,
        visibility_definition_id: DefinitionId,
    ) {
        self.ancestor_dependent_visibilities
            .insert(owner_id, visibility_definition_id);
        self.ancestor_dependent_visibilities_by_member.insert(
            MethodVisibilityDependencyKey::new(owner_id, str_id),
            visibility_definition_id,
        );
        self.debug_assert_consistent();
    }

    pub(crate) fn take_ancestor_dependent_visibility_ids_for_owner(
        &mut self,
        owner_id: DeclarationId,
    ) -> Vec<DefinitionId> {
        let visibility_ids = self.ancestor_dependent_visibilities.remove_key(owner_id);
        for visibility_id in &visibility_ids {
            self.ancestor_dependent_visibilities_by_member
                .remove_value(*visibility_id);
        }
        self.debug_assert_consistent();
        visibility_ids
    }

    pub(crate) fn take_ancestor_dependent_visibility_ids_for_owner_and_member(
        &mut self,
        owner_id: DeclarationId,
        str_id: StringId,
    ) -> Vec<DefinitionId> {
        let visibility_ids = self
            .ancestor_dependent_visibilities_by_member
            .remove_key(MethodVisibilityDependencyKey::new(owner_id, str_id));
        for visibility_id in &visibility_ids {
            self.ancestor_dependent_visibilities.remove_value(*visibility_id);
        }
        self.debug_assert_consistent();
        visibility_ids
    }

    pub(crate) fn detach_ancestor_dependent_visibility(&mut self, visibility_definition_id: DefinitionId) {
        self.ancestor_dependent_visibilities
            .remove_value(visibility_definition_id);
        self.ancestor_dependent_visibilities_by_member
            .remove_value(visibility_definition_id);
        self.debug_assert_consistent();
    }

    pub(crate) fn attach_document_owned_visibility(
        &mut self,
        source_definition_id: DefinitionId,
        visibility_definition_id: DefinitionId,
    ) {
        self.document_owned_visibilities
            .insert(source_definition_id, visibility_definition_id);
        self.debug_assert_consistent();
    }

    pub(crate) fn copy_ids_for_source(&self, source_definition_id: DefinitionId) -> Vec<DefinitionId> {
        self.generated.ids_for_source(source_definition_id)
    }

    pub(crate) fn copy_ids_for_visibility(&self, visibility_definition_id: DefinitionId) -> Vec<DefinitionId> {
        self.generated.ids_for_trigger(visibility_definition_id)
    }

    pub(crate) fn take_document_owned_visibility_ids_for_source(
        &mut self,
        source_definition_id: DefinitionId,
    ) -> Vec<DefinitionId> {
        let visibility_ids = self.document_owned_visibilities.remove_key(source_definition_id);
        self.debug_assert_consistent();
        visibility_ids
    }

    pub(crate) fn detach_visibility(&mut self, visibility_definition_id: DefinitionId) -> Vec<DefinitionId> {
        let copy_definition_ids = self.generated.detach_trigger(visibility_definition_id);
        self.debug_assert_consistent();
        copy_definition_ids
    }

    pub(crate) fn detach_document_owned_visibility(&mut self, visibility_definition_id: DefinitionId) {
        self.document_owned_visibilities.remove_value(visibility_definition_id);
        self.debug_assert_consistent();
    }

    pub(crate) fn remove(&mut self, copy_definition_id: DefinitionId) -> Option<GeneratedMethodCopy> {
        let definition = self.generated.remove(copy_definition_id);
        self.debug_assert_consistent();
        definition
    }

    pub(crate) fn has_ancestor_dependent_visibilities(&self) -> bool {
        !self.ancestor_dependent_visibilities.is_empty()
    }

    fn debug_assert_consistent(&self) {
        self.generated.debug_assert_consistent();
        self.alias_dependent_visibilities.debug_assert_consistent();
        self.ancestor_dependent_visibilities.debug_assert_consistent();
        self.ancestor_dependent_visibilities_by_member.debug_assert_consistent();
        self.document_owned_visibilities.debug_assert_consistent();
    }
}

#[derive(Default, Debug)]
pub(crate) struct AppliedMethodVisibilities {
    by_owner_and_member: BiMultiMap<MethodVisibilityDependencyKey, DefinitionId>,
}

impl AppliedMethodVisibilities {
    pub(crate) fn track(&mut self, owner_id: DeclarationId, str_id: StringId, visibility_definition_id: DefinitionId) {
        self.by_owner_and_member.insert(
            MethodVisibilityDependencyKey::new(owner_id, str_id),
            visibility_definition_id,
        );
        self.debug_assert_consistent();
    }

    pub(crate) fn take_for_owner_and_member(&mut self, owner_id: DeclarationId, str_id: StringId) -> Vec<DefinitionId> {
        let visibility_ids = self
            .by_owner_and_member
            .remove_key(MethodVisibilityDependencyKey::new(owner_id, str_id));
        self.debug_assert_consistent();
        visibility_ids
    }

    pub(crate) fn detach(&mut self, visibility_definition_id: DefinitionId) {
        self.by_owner_and_member.remove_value(visibility_definition_id);
        self.debug_assert_consistent();
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.by_owner_and_member.is_empty()
    }

    fn debug_assert_consistent(&self) {
        self.by_owner_and_member.debug_assert_consistent();
    }

    #[cfg(test)]
    pub(crate) fn assert_visibility_definitions_exist(&self, definitions: &IdentityHashMap<DefinitionId, Definition>) {
        for visibility_id in self.by_owner_and_member.values() {
            assert!(
                matches!(definitions.get(&visibility_id), Some(Definition::MethodVisibility(_))),
                "applied method visibility index references a missing or non-visibility definition"
            );
        }
    }
}

/// Reverse index for method visibility definitions that failed because their target did not exist yet.
///
/// These are retried when the owning namespace or one of its ancestors changes, because a previously missing inherited
/// method can become available incrementally.
#[derive(Default, Debug)]
pub(crate) struct UnresolvedMethodVisibilities {
    by_owner: IdentityHashMap<DeclarationId, Vec<DefinitionId>>,
    by_owner_and_member: IdentityHashMap<MethodVisibilityDependencyKey, Vec<DefinitionId>>,
    owner_by_visibility: IdentityHashMap<DefinitionId, MethodVisibilityDependencyKey>,
}

impl UnresolvedMethodVisibilities {
    pub(crate) fn track(
        &mut self,
        owner_id: DeclarationId,
        str_id: StringId,
        visibility_definition_id: DefinitionId,
    ) -> Option<DeclarationId> {
        let key = MethodVisibilityDependencyKey::new(owner_id, str_id);
        let old_key = self
            .owner_by_visibility
            .insert(visibility_definition_id, key)
            .filter(|old_key| *old_key != key);

        if let Some(old_key) = old_key {
            Self::remove_from_owner_map(&mut self.by_owner, old_key.owner_id, visibility_definition_id);
            Self::remove_from_owner_and_member_map(&mut self.by_owner_and_member, old_key, visibility_definition_id);
        }

        let visibility_ids = self.by_owner.entry(owner_id).or_default();
        push_unique(visibility_ids, visibility_definition_id);

        let visibility_ids = self.by_owner_and_member.entry(key).or_default();
        push_unique(visibility_ids, visibility_definition_id);

        self.debug_assert_consistent();
        old_key.map(|key| key.owner_id)
    }

    pub(crate) fn remove(&mut self, visibility_definition_id: DefinitionId) -> Option<DeclarationId> {
        let key = self.owner_by_visibility.remove(&visibility_definition_id)?;

        Self::remove_from_owner_map(&mut self.by_owner, key.owner_id, visibility_definition_id);
        Self::remove_from_owner_and_member_map(&mut self.by_owner_and_member, key, visibility_definition_id);

        self.debug_assert_consistent();
        Some(key.owner_id)
    }

    pub(crate) fn take_for_owner(&mut self, owner_id: DeclarationId) -> Vec<DefinitionId> {
        let visibility_ids = self.by_owner.remove(&owner_id).unwrap_or_default();

        for visibility_id in &visibility_ids {
            if let Some(key) = self.owner_by_visibility.remove(visibility_id) {
                Self::remove_from_owner_and_member_map(&mut self.by_owner_and_member, key, *visibility_id);
            }
        }

        self.debug_assert_consistent();
        visibility_ids
    }

    pub(crate) fn take_for_owner_and_member(&mut self, owner_id: DeclarationId, str_id: StringId) -> Vec<DefinitionId> {
        let key = MethodVisibilityDependencyKey::new(owner_id, str_id);
        let visibility_ids = self.by_owner_and_member.remove(&key).unwrap_or_default();

        for visibility_id in &visibility_ids {
            if self.owner_by_visibility.remove(visibility_id).is_some() {
                Self::remove_from_owner_map(&mut self.by_owner, owner_id, *visibility_id);
            }
        }

        self.debug_assert_consistent();
        visibility_ids
    }

    fn remove_from_owner_map(
        by_owner: &mut IdentityHashMap<DeclarationId, Vec<DefinitionId>>,
        owner_id: DeclarationId,
        visibility_definition_id: DefinitionId,
    ) {
        if let Some(visibility_ids) = by_owner.get_mut(&owner_id) {
            visibility_ids.retain(|id| *id != visibility_definition_id);
            if visibility_ids.is_empty() {
                by_owner.remove(&owner_id);
            }
        }
    }

    fn remove_from_owner_and_member_map(
        by_owner_and_member: &mut IdentityHashMap<MethodVisibilityDependencyKey, Vec<DefinitionId>>,
        key: MethodVisibilityDependencyKey,
        visibility_definition_id: DefinitionId,
    ) {
        if let Some(visibility_ids) = by_owner_and_member.get_mut(&key) {
            visibility_ids.retain(|id| *id != visibility_definition_id);
            if visibility_ids.is_empty() {
                by_owner_and_member.remove(&key);
            }
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.by_owner.is_empty()
    }

    fn debug_assert_consistent(&self) {
        #[cfg(debug_assertions)]
        {
            for (owner_id, visibility_ids) in &self.by_owner {
                for visibility_id in visibility_ids {
                    debug_assert!(
                        self.owner_by_visibility
                            .get(visibility_id)
                            .is_some_and(|key| key.owner_id == *owner_id),
                        "unresolved visibility missing owner reverse edge"
                    );
                }
            }

            for (key, visibility_ids) in &self.by_owner_and_member {
                for visibility_id in visibility_ids {
                    debug_assert!(
                        self.owner_by_visibility
                            .get(visibility_id)
                            .is_some_and(|stored_key| stored_key == key),
                        "unresolved visibility missing owner/member reverse edge"
                    );
                }
            }

            for (visibility_id, key) in &self.owner_by_visibility {
                debug_assert!(
                    self.by_owner
                        .get(&key.owner_id)
                        .is_some_and(|visibility_ids| visibility_ids.contains(visibility_id)),
                    "unresolved visibility missing owner forward edge"
                );
                debug_assert!(
                    self.by_owner_and_member
                        .get(key)
                        .is_some_and(|visibility_ids| visibility_ids.contains(visibility_id)),
                    "unresolved visibility missing owner/member forward edge"
                );
            }
        }
    }

    #[cfg(test)]
    pub(crate) fn assert_visibility_definitions_exist(&self, definitions: &IdentityHashMap<DefinitionId, Definition>) {
        for visibility_id in self.owner_by_visibility.keys() {
            assert!(
                matches!(definitions.get(visibility_id), Some(Definition::MethodVisibility(_))),
                "unresolved method visibility index references a missing or non-visibility definition"
            );
        }
    }
}
