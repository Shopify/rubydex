use std::{collections::HashMap, rc::Rc};

use super::Resolver;
use crate::model::{
    declaration::{Ancestor, Declaration},
    definitions::Definition,
    graph::DefinitionProgramOrderKey,
    identity_maps::IdentityHashSet,
    ids::{DeclarationId, DefinitionId, StringId, UriId},
    visibility::Visibility,
};
use crate::offset::Offset;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct DefinitionOrderKey {
    method_id: DeclarationId,
    uri_id: UriId,
    // Same-file "before" checks use the visibility start offset only; the
    // end offset does not affect membership in this cache bucket.
    offset_start: u32,
}

#[derive(Default)]
pub(in crate::resolution::method_visibility) struct DefinitionOrderCache {
    ids_by_position: HashMap<DefinitionOrderKey, Rc<[DefinitionId]>>,
}

impl Resolver<'_> {
    pub(in crate::resolution::method_visibility) fn method_visibility_before_visibility_definition(
        &self,
        method_id: DeclarationId,
        visibility_id: DefinitionId,
        definition_order_cache: &mut DefinitionOrderCache,
    ) -> Option<Visibility> {
        let visibility_definition = self.graph.definitions().get(&visibility_id)?;
        let mut seen_method_ids = IdentityHashSet::default();
        self.method_visibility_before(
            method_id,
            *visibility_definition.uri_id(),
            visibility_definition.offset(),
            &mut seen_method_ids,
            definition_order_cache,
        )
    }

    fn method_visibility_before(
        &self,
        method_id: DeclarationId,
        uri_id: UriId,
        offset: &Offset,
        seen_method_ids: &mut IdentityHashSet<DeclarationId>,
        definition_order_cache: &mut DefinitionOrderCache,
    ) -> Option<Visibility> {
        if !seen_method_ids.insert(method_id) {
            return None;
        }

        for definition_id in self
            .definition_ids_before_visibility(method_id, uri_id, offset, definition_order_cache)?
            .iter()
            .copied()
            .rev()
        {
            let definition = self.graph.definitions().get(&definition_id).unwrap();

            if let Some(visibility) = definition.method_effective_visibility() {
                return Some(visibility);
            }

            let Definition::MethodAlias(alias) = definition else {
                continue;
            };

            let owner_id = self
                .graph
                .definition_id_to_declaration_id(definition_id)
                .and_then(|decl_id| self.graph.declarations().get(decl_id))
                .map(|declaration| *declaration.owner_id())?;
            for target_id in self.method_member_ids_in_ancestors_before(
                owner_id,
                *alias.old_name_str_id(),
                *definition.uri_id(),
                definition.offset(),
                definition_order_cache,
            ) {
                let Some(visibility) = self.method_visibility_before(
                    target_id,
                    *definition.uri_id(),
                    definition.offset(),
                    seen_method_ids,
                    definition_order_cache,
                ) else {
                    continue;
                };
                return Some(visibility);
            }
        }

        Some(Visibility::Public)
    }

    pub(in crate::resolution::method_visibility) fn method_has_definition_before_visibility(
        &self,
        method_id: DeclarationId,
        visibility_id: DefinitionId,
        definition_order_cache: &mut DefinitionOrderCache,
    ) -> bool {
        let Some(visibility_definition) = self.graph.definitions().get(&visibility_id) else {
            return false;
        };

        self.method_has_definition_before_position(
            method_id,
            *visibility_definition.uri_id(),
            visibility_definition.offset(),
            definition_order_cache,
        )
    }

    fn method_has_definition_before_position(
        &self,
        method_id: DeclarationId,
        uri_id: UriId,
        offset: &Offset,
        definition_order_cache: &mut DefinitionOrderCache,
    ) -> bool {
        self.definition_ids_before_visibility(method_id, uri_id, offset, definition_order_cache)
            .is_some_and(|definition_ids| {
                definition_ids.iter().copied().any(|definition_id| {
                    let Some(definition) = self.graph.definitions().get(&definition_id) else {
                        return false;
                    };

                    definition.establishes_method_member()
                })
            })
    }

    pub(in crate::resolution::method_visibility) fn method_member_ids_in_ancestors_before(
        &self,
        owner_id: DeclarationId,
        str_id: StringId,
        uri_id: UriId,
        offset: &Offset,
        definition_order_cache: &mut DefinitionOrderCache,
    ) -> Vec<DeclarationId> {
        let Some(namespace) = self
            .graph
            .declarations()
            .get(&owner_id)
            .and_then(Declaration::as_namespace)
        else {
            return Vec::new();
        };

        namespace
            .ancestors()
            .iter()
            .filter_map(|ancestor| {
                let Ancestor::Complete(ancestor_id) = ancestor else {
                    return None;
                };
                let member_id = self
                    .graph
                    .declarations()
                    .get(ancestor_id)?
                    .as_namespace()?
                    .member(&str_id)
                    .copied()?;

                if !matches!(self.graph.declarations().get(&member_id), Some(Declaration::Method(_))) {
                    return None;
                }

                self.method_has_definition_before_position(member_id, uri_id, offset, definition_order_cache)
                    .then_some(member_id)
            })
            .collect()
    }

    pub(in crate::resolution::method_visibility) fn definition_ids_before_visibility(
        &self,
        method_id: DeclarationId,
        visibility_uri_id: UriId,
        visibility_offset: &Offset,
        cache: &mut DefinitionOrderCache,
    ) -> Option<Rc<[DefinitionId]>> {
        let key = DefinitionOrderKey {
            method_id,
            uri_id: visibility_uri_id,
            offset_start: visibility_offset.start(),
        };
        if let Some(definitions) = cache.ids_by_position.get(&key) {
            return Some(Rc::clone(definitions));
        }

        let mut definitions = self
            .graph
            .declarations()
            .get(&method_id)?
            .definitions()
            .iter()
            .copied()
            .filter_map(|definition_id| {
                let (same_uri, uri, offset) = self.graph.definition_order_key_before_position(
                    definition_id,
                    visibility_uri_id,
                    visibility_offset,
                )?;
                Some((definition_id, same_uri, uri, offset))
            })
            .collect::<Vec<_>>();

        definitions.sort_unstable_by(
            |(left_id, left_same_uri, left_uri, left_offset), (right_id, right_same_uri, right_uri, right_offset)| {
                // Cross-file candidates sort before same-file candidates so the reverse scans below prefer
                // definitions known to be before the visibility call in the same file.
                left_same_uri.cmp(right_same_uri).then_with(|| {
                    DefinitionProgramOrderKey::new(*left_id, left_uri, left_offset)
                        .cmp(&DefinitionProgramOrderKey::new(*right_id, right_uri, right_offset))
                })
            },
        );

        let definitions = definitions
            .into_iter()
            .map(|(definition_id, _, _, _)| definition_id)
            .collect::<Vec<_>>();
        let definitions = Rc::<[DefinitionId]>::from(definitions.into_boxed_slice());
        cache.ids_by_position.insert(key, Rc::clone(&definitions));
        Some(definitions)
    }
}
