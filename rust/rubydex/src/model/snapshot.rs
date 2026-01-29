use crate::model::{
    definitions::{Definition, Mixin},
    graph::Graph,
    identity_maps::{IdentityHashMap, IdentityHashSet},
    ids::{DeclarationId, NameId, ReferenceId, StringId, UriId},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MixinKind {
    Include,
    Prepend,
    Extend,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MixinSnapshot {
    pub kind: MixinKind,
    pub name: String,
}

#[derive(Debug, Default)]
pub struct DocumentSnapshot {
    pub members: IdentityHashMap<DeclarationId, Vec<StringId>>,
    pub mixins: IdentityHashMap<DeclarationId, Vec<MixinSnapshot>>,
    pub superclasses: IdentityHashMap<DeclarationId, String>,
    pub alias_targets: IdentityHashMap<DeclarationId, NameId>,
    pub reference_ids: Vec<ReferenceId>,
}

#[derive(Debug, Default)]
pub struct SnapshotDiff {
    pub changed_members: IdentityHashMap<DeclarationId, IdentityHashSet<StringId>>,
    pub changed_ancestors: IdentityHashSet<DeclarationId>,
    pub added_references: Vec<ReferenceId>,
    pub removed_references: Vec<ReferenceId>,
}

impl DocumentSnapshot {
    #[must_use]
    pub fn capture(graph: &Graph, uri_id: UriId) -> Self {
        let mut snapshot = Self::default();
        let mut members_with_offsets: IdentityHashMap<DeclarationId, Vec<(StringId, u32)>> = IdentityHashMap::default();

        let Some(document) = graph.documents().get(&uri_id) else {
            return snapshot;
        };

        snapshot.reference_ids = document.constant_references().to_vec();

        for &def_id in document.definitions() {
            let Some(definition) = graph.definitions().get(&def_id) else {
                continue;
            };

            let offset = definition.offset().start();

            match definition {
                Definition::Class(class_def) => {
                    let name_id = class_def.name_id();
                    if let Some(&decl_id) = graph.name_id_to_declaration_id(*name_id) {
                        if let Some(decl) = graph.declarations().get(&decl_id)
                            && let Some(name_ref) = graph.names().get(name_id)
                        {
                            let owner_id = *decl.owner_id();
                            let str_id = *name_ref.str();
                            members_with_offsets.entry(owner_id).or_default().push((str_id, offset));
                        }

                        for mixin in class_def.mixins() {
                            if let Some(mixin_snapshot) = mixin_to_snapshot(graph, mixin) {
                                snapshot.mixins.entry(decl_id).or_default().push(mixin_snapshot);
                            }
                        }
                        if let Some(superclass_ref) = class_def.superclass_ref()
                            && let Some(name) = resolve_ref_to_name(graph, *superclass_ref)
                        {
                            snapshot.superclasses.insert(decl_id, name);
                        }
                    }
                }
                Definition::Module(module_def) => {
                    let name_id = module_def.name_id();
                    if let Some(&decl_id) = graph.name_id_to_declaration_id(*name_id) {
                        if let Some(decl) = graph.declarations().get(&decl_id)
                            && let Some(name_ref) = graph.names().get(name_id)
                        {
                            let owner_id = *decl.owner_id();
                            let str_id = *name_ref.str();
                            members_with_offsets.entry(owner_id).or_default().push((str_id, offset));
                        }

                        for mixin in module_def.mixins() {
                            if let Some(mixin_snapshot) = mixin_to_snapshot(graph, mixin) {
                                snapshot.mixins.entry(decl_id).or_default().push(mixin_snapshot);
                            }
                        }
                    }
                }
                Definition::Constant(const_def) => {
                    let name_id = const_def.name_id();
                    if let Some(&decl_id) = graph.name_id_to_declaration_id(*name_id)
                        && let Some(decl) = graph.declarations().get(&decl_id)
                        && let Some(name_ref) = graph.names().get(name_id)
                    {
                        let owner_id = *decl.owner_id();
                        let str_id = *name_ref.str();
                        members_with_offsets.entry(owner_id).or_default().push((str_id, offset));
                    }
                }
                Definition::ConstantAlias(alias_def) => {
                    let name_id = alias_def.name_id();
                    if let Some(&decl_id) = graph.name_id_to_declaration_id(*name_id) {
                        if let Some(decl) = graph.declarations().get(&decl_id)
                            && let Some(name_ref) = graph.names().get(name_id)
                        {
                            let owner_id = *decl.owner_id();
                            let str_id = *name_ref.str();
                            members_with_offsets.entry(owner_id).or_default().push((str_id, offset));
                        }
                        snapshot.alias_targets.insert(decl_id, *alias_def.target_name_id());
                    }
                }
                _ => {}
            }
        }

        for (decl_id, mut members) in members_with_offsets {
            members.sort_by_key(|(_, offset)| *offset);
            snapshot
                .members
                .insert(decl_id, members.into_iter().map(|(str_id, _)| str_id).collect());
        }

        snapshot
    }

    #[must_use]
    pub fn diff(&self, new: &Self) -> SnapshotDiff {
        let mut diff = SnapshotDiff::default();

        let all_decl_ids: IdentityHashSet<DeclarationId> = self
            .members
            .keys()
            .chain(new.members.keys())
            .chain(self.mixins.keys())
            .chain(new.mixins.keys())
            .chain(self.superclasses.keys())
            .chain(new.superclasses.keys())
            .copied()
            .collect();

        for decl_id in all_decl_ids {
            let old_members = self.members.get(&decl_id);
            let new_members = new.members.get(&decl_id);

            if old_members != new_members {
                let changed_names: IdentityHashSet<StringId> = old_members
                    .into_iter()
                    .flatten()
                    .chain(new_members.into_iter().flatten())
                    .copied()
                    .collect();

                if !changed_names.is_empty() {
                    diff.changed_members.insert(decl_id, changed_names);
                }
            }

            let old_mixins = self.mixins.get(&decl_id);
            let new_mixins = new.mixins.get(&decl_id);
            let old_super = self.superclasses.get(&decl_id);
            let new_super = new.superclasses.get(&decl_id);

            if old_mixins != new_mixins || old_super != new_super {
                diff.changed_ancestors.insert(decl_id);
            }
        }

        let old_refs: IdentityHashSet<ReferenceId> = self.reference_ids.iter().copied().collect();
        let new_refs: IdentityHashSet<ReferenceId> = new.reference_ids.iter().copied().collect();

        diff.added_references = new_refs.difference(&old_refs).copied().collect();
        diff.removed_references = old_refs.difference(&new_refs).copied().collect();

        diff
    }
}

#[must_use]
#[allow(clippy::implicit_hasher)]
pub fn find_affected_references(
    graph: &Graph,
    diff: &SnapshotDiff,
    nesting_index: &IdentityHashMap<DeclarationId, Vec<ReferenceId>>,
) -> IdentityHashSet<ReferenceId> {
    let mut affected: IdentityHashSet<ReferenceId> = IdentityHashSet::default();

    affected.extend(diff.added_references.iter().copied());

    for (&decl_id, changed_names) in &diff.changed_members {
        refs_affected_by_member_change(graph, decl_id, changed_names, nesting_index, &mut affected);
    }

    for &decl_id in &diff.changed_ancestors {
        refs_affected_by_ancestor_change(graph, decl_id, nesting_index, &mut affected);
    }

    affected
}

fn refs_affected_by_member_change(
    graph: &Graph,
    decl_id: DeclarationId,
    changed_names: &IdentityHashSet<StringId>,
    nesting_index: &IdentityHashMap<DeclarationId, Vec<ReferenceId>>,
    affected: &mut IdentityHashSet<ReferenceId>,
) {
    collect_refs_for_decl_and_descendants(graph, decl_id, nesting_index, affected, |ref_id| {
        ref_matches_names(graph, ref_id, changed_names)
    });
}

fn refs_affected_by_ancestor_change(
    graph: &Graph,
    decl_id: DeclarationId,
    nesting_index: &IdentityHashMap<DeclarationId, Vec<ReferenceId>>,
    affected: &mut IdentityHashSet<ReferenceId>,
) {
    collect_refs_for_decl_and_descendants(graph, decl_id, nesting_index, affected, |_| true);
}

fn collect_refs_for_decl_and_descendants<F>(
    graph: &Graph,
    decl_id: DeclarationId,
    nesting_index: &IdentityHashMap<DeclarationId, Vec<ReferenceId>>,
    affected: &mut IdentityHashSet<ReferenceId>,
    filter: F,
) where
    F: Fn(ReferenceId) -> bool,
{
    if let Some(refs) = nesting_index.get(&decl_id) {
        for &ref_id in refs {
            if filter(ref_id) {
                affected.insert(ref_id);
            }
        }
    }

    let Some(decl) = graph.declarations().get(&decl_id) else {
        return;
    };
    let Some(namespace) = decl.as_namespace() else {
        return;
    };

    if let Some(singleton_id) = namespace.singleton_class()
        && let Some(refs) = nesting_index.get(singleton_id)
    {
        for &ref_id in refs {
            if filter(ref_id) {
                affected.insert(ref_id);
            }
        }
    }

    namespace.for_each_descendant(|descendant_id| {
        if let Some(refs) = nesting_index.get(descendant_id) {
            for &ref_id in refs {
                if filter(ref_id) {
                    affected.insert(ref_id);
                }
            }
        }
    });
}

fn ref_matches_names(graph: &Graph, ref_id: ReferenceId, names: &IdentityHashSet<StringId>) -> bool {
    let Some(constant_ref) = graph.constant_references().get(&ref_id) else {
        return false;
    };
    let Some(name_ref) = graph.names().get(constant_ref.name_id()) else {
        return false;
    };
    names.contains(name_ref.str())
}

fn mixin_to_snapshot(graph: &Graph, mixin: &Mixin) -> Option<MixinSnapshot> {
    let (kind, ref_id) = match mixin {
        Mixin::Include(def) => (MixinKind::Include, *def.constant_reference_id()),
        Mixin::Prepend(def) => (MixinKind::Prepend, *def.constant_reference_id()),
        Mixin::Extend(def) => (MixinKind::Extend, *def.constant_reference_id()),
    };
    let name = resolve_ref_to_name(graph, ref_id)?;
    Some(MixinSnapshot { kind, name })
}

fn resolve_ref_to_name(graph: &Graph, ref_id: ReferenceId) -> Option<String> {
    let const_ref = graph.constant_references().get(&ref_id)?;
    let name_ref = graph.names().get(const_ref.name_id())?;
    let str_ref = graph.strings().get(name_ref.str())?;
    Some(str_ref.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::ids::UriId;
    use crate::test_utils::GraphTest;

    #[test]
    fn snapshot_captures_members() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "module Foo; class Bar; end; end");
        context.resolve();

        let snapshot = DocumentSnapshot::capture(context.graph(), UriId::from("file:///foo.rb"));

        assert!(!snapshot.members.is_empty());
    }

    #[test]
    fn diff_detects_added_references() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "module Foo; end");
        context.resolve();

        let old_snapshot = DocumentSnapshot::capture(context.graph(), UriId::from("file:///foo.rb"));

        context.index_uri("file:///foo.rb", "module Foo; Bar; end");
        context.resolve();

        let new_snapshot = DocumentSnapshot::capture(context.graph(), UriId::from("file:///foo.rb"));
        let diff = old_snapshot.diff(&new_snapshot);

        assert!(!diff.added_references.is_empty());
    }

    #[test]
    fn diff_detects_member_changes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "module Foo; end");
        context.resolve();

        let old_snapshot = DocumentSnapshot::capture(context.graph(), UriId::from("file:///foo.rb"));

        context.index_uri("file:///foo.rb", "module Foo; class Bar; end; end");
        context.resolve();

        let new_snapshot = DocumentSnapshot::capture(context.graph(), UriId::from("file:///foo.rb"));
        let diff = old_snapshot.diff(&new_snapshot);

        assert!(!diff.changed_members.is_empty());
    }
}
