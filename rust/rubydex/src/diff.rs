use std::collections::HashSet;

use crate::model::{
    graph::Graph,
    ids::{DeclarationId, DefinitionId, NameId, ReferenceId},
    name::NameRef,
};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct GraphDiff {
    pub added_declarations: HashSet<DeclarationId>,
    pub removed_declarations: HashSet<DeclarationId>,
    pub changed_declarations: HashSet<DeclarationId>,
    pub added_definitions: HashSet<DefinitionId>,
    pub removed_definitions: HashSet<DefinitionId>,
    pub added_references: HashSet<ReferenceId>,
    pub removed_references: HashSet<ReferenceId>,
    pub added_names: HashSet<NameId>,
    pub removed_names: HashSet<NameId>,
    pub changed_names: HashSet<NameId>,
}

impl GraphDiff {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.added_declarations.is_empty()
            && self.removed_declarations.is_empty()
            && self.changed_declarations.is_empty()
            && self.added_definitions.is_empty()
            && self.removed_definitions.is_empty()
            && self.added_references.is_empty()
            && self.removed_references.is_empty()
            && self.added_names.is_empty()
            && self.removed_names.is_empty()
            && self.changed_names.is_empty()
    }
}

fn declarations_equal(a: &Graph, b: &Graph, id: DeclarationId) -> bool {
    let (Some(decl_a), Some(decl_b)) = (a.declarations().get(&id), b.declarations().get(&id)) else {
        return false;
    };

    let Some(ns_a) = decl_a.as_namespace() else {
        return true;
    };
    let Some(ns_b) = decl_b.as_namespace() else {
        return true;
    };

    ns_a.members() == ns_b.members()
        && ns_a.ancestors().iter().collect::<Vec<_>>() == ns_b.ancestors().iter().collect::<Vec<_>>()
        && ns_a.descendants() == ns_b.descendants()
        && ns_a.singleton_class() == ns_b.singleton_class()
}

fn names_equal(a: &Graph, b: &Graph, id: NameId) -> bool {
    let (Some(name_a), Some(name_b)) = (a.names().get(&id), b.names().get(&id)) else {
        return false;
    };

    match (name_a, name_b) {
        (NameRef::Resolved(a), NameRef::Resolved(b)) => a.declaration_id() == b.declaration_id(),
        (NameRef::Unresolved(_), NameRef::Unresolved(_)) => true,
        _ => false,
    }
}

#[must_use]
pub fn diff(a: &Graph, b: &Graph) -> Option<GraphDiff> {
    let mut result = GraphDiff::default();

    for id in a.declarations().keys() {
        if !b.declarations().contains_key(id) {
            result.removed_declarations.insert(*id);
        } else if !declarations_equal(a, b, *id) {
            result.changed_declarations.insert(*id);
        }
    }
    for id in b.declarations().keys() {
        if !a.declarations().contains_key(id) {
            result.added_declarations.insert(*id);
        }
    }

    for id in a.definitions().keys() {
        if !b.definitions().contains_key(id) {
            result.removed_definitions.insert(*id);
        }
    }
    for id in b.definitions().keys() {
        if !a.definitions().contains_key(id) {
            result.added_definitions.insert(*id);
        }
    }

    for id in a.constant_references().keys() {
        if !b.constant_references().contains_key(id) {
            result.removed_references.insert(*id);
        }
    }
    for id in b.constant_references().keys() {
        if !a.constant_references().contains_key(id) {
            result.added_references.insert(*id);
        }
    }

    for id in a.names().keys() {
        if !b.names().contains_key(id) {
            result.removed_names.insert(*id);
        } else if !names_equal(a, b, *id) {
            result.changed_names.insert(*id);
        }
    }
    for id in b.names().keys() {
        if !a.names().contains_key(id) {
            result.added_names.insert(*id);
        }
    }

    if result.is_empty() { None } else { Some(result) }
}
