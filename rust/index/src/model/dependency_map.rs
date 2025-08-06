use std::collections::{HashMap, HashSet};

use crate::model::ids::DependencyId;

// A dependency graph that can store different types of relationships between Ruby entities
#[derive(Default, Debug)]
pub struct DependencyMap {
    // Maps an entity to every other entity it depends on
    depends_on: HashMap<DependencyId, HashSet<DependencyId>>,
    // Reverse mapping: maps an entity to every other entity that depends on it
    depended_by: HashMap<DependencyId, HashSet<DependencyId>>,
}

impl DependencyMap {
    #[must_use]
    pub fn new() -> Self {
        Self {
            depends_on: HashMap::new(),
            depended_by: HashMap::new(),
        }
    }

    pub fn declare_dependency(&mut self, from: DependencyId, to: DependencyId) {
        self.depends_on.entry(from).or_default().insert(to);
        self.depended_by.entry(to).or_default().insert(from);
    }

    /// Remove a dependency from the graph. This method returns a vector of dependency IDs that are no longer reacheable
    /// in the graph. Nothing depends on them anymore and so they can be safely removed from the Index representation as
    /// well
    ///
    /// # Panics
    ///
    /// This method only panics if there's data inconsistency in the dependency map, which indicates a bug in the
    /// implementation
    pub fn remove_dependency(&mut self, dependency_id: &DependencyId) -> Vec<DependencyId> {
        let mut unreacheable_dependencies = Vec::new();

        if let Some(ids) = self.depends_on.remove(dependency_id) {
            for other_id in ids {
                let dependents = self
                    .depended_by
                    .get_mut(&other_id)
                    .expect("Data inconsistency bug: ID exists in `depends_on`, but not in `depended_by`");

                dependents.remove(dependency_id);

                if dependents.is_empty() {
                    self.depended_by.remove(&other_id);
                    unreacheable_dependencies.push(other_id);
                }
            }
        }

        unreacheable_dependencies
    }

    #[must_use]
    pub fn iter_depends_on(&self, dependency_id: &DependencyId) -> Option<impl Iterator<Item = &DependencyId>> {
        let dependencies = self.depends_on.get(dependency_id)?;
        Some(dependencies.iter())
    }

    #[must_use]
    pub fn iter_depended_by(&self, dependency_id: &DependencyId) -> Option<impl Iterator<Item = &DependencyId>> {
        let dependencies = self.depended_by.get(dependency_id)?;
        Some(dependencies.iter())
    }
}

#[cfg(test)]
mod tests {
    use crate::model::ids::DeclarationId;

    use super::*;

    #[test]
    fn iterating_over_depends_on() {
        let mut map = DependencyMap::new();
        let bar_id = DependencyId::Declaration(DeclarationId::new("Bar"));
        let foo_id = DependencyId::Declaration(DeclarationId::new("Foo"));
        map.declare_dependency(foo_id, bar_id);

        let dependencies: Vec<&DependencyId> = map.iter_depends_on(&foo_id).unwrap().collect();
        assert_eq!(dependencies.len(), 1);
        assert_eq!(dependencies[0], &bar_id);
    }

    #[test]
    fn iterating_over_depended_by() {
        let mut map = DependencyMap::new();
        let bar_id = DependencyId::Declaration(DeclarationId::new("Bar"));
        let foo_id = DependencyId::Declaration(DeclarationId::new("Foo"));
        map.declare_dependency(foo_id, bar_id);

        let dependencies: Vec<&DependencyId> = map.iter_depended_by(&bar_id).unwrap().collect();
        assert_eq!(dependencies.len(), 1);
        assert_eq!(dependencies[0], &foo_id);
    }

    #[test]
    fn removing_a_dependency() {
        let mut map = DependencyMap::new();
        let bar_id = DependencyId::Declaration(DeclarationId::new("Bar"));
        let foo_id = DependencyId::Declaration(DeclarationId::new("Foo"));
        map.declare_dependency(foo_id, bar_id);
        assert_eq!(map.depended_by.len(), 1);
        assert_eq!(map.depends_on.len(), 1);

        map.remove_dependency(&foo_id);
        assert!(map.depended_by.is_empty());
        assert!(map.depends_on.is_empty());
    }
}
