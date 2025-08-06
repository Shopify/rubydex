use std::collections::HashMap;

use crate::indexing::indexed_data::{IndexingThreadData, PartialDeclaration};
use crate::model::declaration::Declaration;
use crate::model::dependency_map::DependencyMap;
use crate::model::ids::{DeclarationId, DependencyId, UriId};

// The `Index` is the global graph representation of the entire Ruby codebase. It contains all declarations and their
// relationships
#[derive(Debug, Default)]
pub struct Index {
    declarations: HashMap<DeclarationId, Declaration>,
    uri_pool: HashMap<UriId, String>,
    dependency_map: DependencyMap,
}

impl Index {
    #[must_use]
    pub fn new() -> Self {
        Self {
            declarations: HashMap::new(),
            uri_pool: HashMap::new(),
            dependency_map: DependencyMap::new(),
        }
    }

    /// # Panics
    ///
    /// This method will panic if there's inconsistency in the index's graph representation, which indicates a bug in
    /// our implementation
    pub fn merge_definitions(&mut self, indexed_data: IndexingThreadData) {
        let (declarations, uris) = indexed_data.into_parts();
        self.uri_pool.extend(uris);

        for (uri_id, mut map) in declarations {
            let uri_dependency_id = DependencyId::Uri(uri_id);
            let dependency_ids: Option<Vec<DependencyId>> = self
                .dependency_map
                .iter_depended_by(&uri_dependency_id)
                .map(|deps| deps.copied().collect());

            match dependency_ids {
                Some(dependents) => {
                    self.handle_existing_declarations(uri_id, &mut map, dependents);
                }
                None => self.insert_new_declarations(uri_id, map),
            }
        }
    }

    fn handle_existing_declarations(
        &mut self,
        uri_id: UriId,
        partial_declaration_map: &mut HashMap<String, PartialDeclaration>,
        dependents: Vec<DependencyId>,
    ) {
        for dependent_id in dependents {
            if let DependencyId::Declaration(id) = dependent_id {
                let declaration = self.declarations.get_mut(&id).unwrap_or_else(|| {
                    panic!("{id} is registered as a dependency, but not present in declarations. This is a bug")
                });

                // If the newly indexed data exists, then we need to update the existing declaration with
                // whatever we discovered. If it does not exist, then all definitions for this URI were
                // removed and we have to do the same in the declaration
                if let Some(partial) = partial_declaration_map.remove(declaration.name()) {
                    let (_id, definitions) = partial.into();
                    declaration.update_definitions(uri_id, definitions);
                } else {
                    declaration.delete_definitions(uri_id);

                    if declaration.is_empty() {
                        self.remove_from_graph(dependent_id);
                    }
                }
            }
        }
    }

    // Removes a dependency_id from the graph, deleting it from its storage container (e.g.: uri or declaration pools)
    // and from all dependency relationships. If as a result of removing `dependency_id`, other entities become
    // unreacheable, they too are removed recursively from the graph.
    //
    // For example, if a file is deleted:
    // 1. Declarations related to the file are removed
    // 2. All dependency relationships for those declarations are removed
    // 3. If no declarations exist for that URI, the URI is removed along with all of its relationships
    fn remove_from_graph(&mut self, dependency_id: DependencyId) {
        // Remove the dependency itself from the Index
        match &dependency_id {
            DependencyId::Declaration(id) => {
                self.declarations.remove(id);
            }
            DependencyId::Uri(id) => {
                self.uri_pool.remove(id);
            }
        }

        // Remove dependency connections. If any entities become unreacheable, recursively remove them
        for unreacheable_dependency_id in self.dependency_map.remove_dependency(&dependency_id) {
            self.remove_from_graph(unreacheable_dependency_id);
        }
    }

    // Inserts new declarations discovered in the URI. This method should only be used when this declaration is brand
    // new and has not been seen before
    fn insert_new_declarations(&mut self, uri_id: UriId, map: HashMap<String, PartialDeclaration>) {
        for (name, declarations) in map {
            let (id, definitions) = declarations.into();

            // Declare the direct and reverse dependencies of this definition on the URI
            self.dependency_map
                .declare_dependency(DependencyId::Declaration(id), DependencyId::Uri(uri_id));

            // Insert the new declaration
            let mut declaration = Declaration::new(name);
            declaration.update_definitions(uri_id, definitions);
            self.declarations.insert(id, declaration);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::indexing::indexed_data::IndexingThreadData;
    use crate::model::definitions::{ClassDefinition, Definition};
    use crate::model::ids::{DeclarationId, DependencyId};
    use crate::offset::Offset;

    #[test]
    fn inserting_new_definitions() {
        let mut index = Index::new();
        let mut data = IndexingThreadData::new();
        let name = "Foo".to_string();
        let decl_id = DeclarationId::new(&name);
        let uri = data.add_uri("file:///foo.rb".to_string());
        data.add_definition(
            uri,
            name,
            Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 5)))),
        );
        index.merge_definitions(data);

        assert!(index.declarations.contains_key(&decl_id));
        let uri_dep = DependencyId::Uri(uri);
        let decl_dep = DependencyId::Declaration(decl_id);
        assert!(
            index
                .dependency_map
                .iter_depended_by(&uri_dep)
                .unwrap()
                .collect::<Vec<_>>()
                .contains(&&decl_dep)
        );
        assert!(
            index
                .dependency_map
                .iter_depends_on(&decl_dep)
                .unwrap()
                .collect::<Vec<_>>()
                .contains(&&uri_dep)
        );

        assert_eq!(index.uri_pool.len(), 1);
        assert_eq!(*index.uri_pool.get(&uri).unwrap(), "file:///foo.rb");
    }

    #[test]
    fn deleting_existing_definitions() {
        let mut index = Index::new();
        let mut data = IndexingThreadData::new();
        let name = "Foo".to_string();
        let decl_id = DeclarationId::new(&name);
        let uri = data.add_uri("file:///foo.rb".to_string());
        data.add_definition(
            uri,
            name,
            Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 5)))),
        );
        index.merge_definitions(data);

        assert!(index.declarations.contains_key(&decl_id));
        let uri_dep = DependencyId::Uri(uri);
        let decl_dep = DependencyId::Declaration(decl_id);

        assert!(
            index
                .dependency_map
                .iter_depended_by(&uri_dep)
                .unwrap()
                .collect::<Vec<_>>()
                .contains(&&decl_dep)
        );
        assert!(
            index
                .dependency_map
                .iter_depends_on(&decl_dep)
                .unwrap()
                .collect::<Vec<_>>()
                .contains(&&uri_dep)
        );

        let mut new_data = IndexingThreadData::new();
        new_data.add_uri("file:///foo.rb".to_string());
        index.merge_definitions(new_data);

        assert!(!index.declarations.contains_key(&decl_id));
        assert!(index.dependency_map.iter_depended_by(&uri_dep).is_none());
        assert!(index.dependency_map.iter_depends_on(&decl_dep).is_none());

        assert_eq!(index.uri_pool.len(), 0);
    }

    #[test]
    fn updating_existing_definitions() {
        let mut index = Index::new();
        let mut data = IndexingThreadData::new();
        let name = "Foo".to_string();
        let decl_id = DeclarationId::new(&name);
        let uri = data.add_uri("file:///foo.rb".to_string());
        data.add_definition(
            uri,
            name,
            Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 5)))),
        );
        index.merge_definitions(data);

        assert!(index.declarations.contains_key(&decl_id));
        let uri_dep = DependencyId::Uri(uri);
        let decl_dep = DependencyId::Declaration(decl_id);

        assert!(
            index
                .dependency_map
                .iter_depended_by(&uri_dep)
                .unwrap()
                .collect::<Vec<_>>()
                .contains(&&decl_dep)
        );
        assert!(
            index
                .dependency_map
                .iter_depends_on(&decl_dep)
                .unwrap()
                .collect::<Vec<_>>()
                .contains(&&uri_dep)
        );

        let mut new_data = IndexingThreadData::new();
        let uri = new_data.add_uri("file:///foo.rb".to_string());
        new_data.add_definition(
            uri,
            "Foo".into(),
            Definition::Class(Box::new(ClassDefinition::new(Offset::new(15, 40)))),
        );
        index.merge_definitions(new_data);

        assert!(index.declarations.contains_key(&decl_id));
        assert!(
            index
                .dependency_map
                .iter_depended_by(&uri_dep)
                .unwrap()
                .collect::<Vec<_>>()
                .contains(&&decl_dep)
        );
        assert!(
            index
                .dependency_map
                .iter_depends_on(&decl_dep)
                .unwrap()
                .collect::<Vec<_>>()
                .contains(&&uri_dep)
        );

        assert_eq!(index.uri_pool.len(), 1);
        assert_eq!(*index.uri_pool.get(&uri).unwrap(), "file:///foo.rb");
    }

    #[test]
    fn inserting_definitions_from_different_uris_to_existing_declaration() {
        let mut index = Index::new();
        let mut data = IndexingThreadData::new();
        let name = "Foo".to_string();
        let decl_id = DeclarationId::new(&name);
        let uri = data.add_uri("file:///foo.rb".to_string());
        data.add_definition(
            uri,
            name,
            Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 5)))),
        );
        index.merge_definitions(data);

        let mut new_data = IndexingThreadData::new();
        let uri_2 = new_data.add_uri("file:///foo2.rb".to_string());
        new_data.add_definition(
            uri_2,
            "Foo".into(),
            Definition::Class(Box::new(ClassDefinition::new(Offset::new(15, 40)))),
        );
        index.merge_definitions(new_data);

        assert!(index.declarations.contains_key(&decl_id));
        let uri_dep_1 = DependencyId::Uri(uri);
        let uri_dep_2 = DependencyId::Uri(uri_2);
        let decl_dep = DependencyId::Declaration(decl_id);

        assert!(
            index
                .dependency_map
                .iter_depended_by(&uri_dep_1)
                .unwrap()
                .collect::<Vec<_>>()
                .contains(&&decl_dep)
        );
        assert!(
            index
                .dependency_map
                .iter_depended_by(&uri_dep_2)
                .unwrap()
                .collect::<Vec<_>>()
                .contains(&&decl_dep)
        );
        assert!(
            index
                .dependency_map
                .iter_depends_on(&decl_dep)
                .unwrap()
                .collect::<Vec<_>>()
                .contains(&&uri_dep_1)
        );
        assert!(
            index
                .dependency_map
                .iter_depends_on(&decl_dep)
                .unwrap()
                .collect::<Vec<_>>()
                .contains(&&uri_dep_2)
        );

        assert_eq!(index.uri_pool.len(), 2);
        assert_eq!(*index.uri_pool.get(&uri).unwrap(), "file:///foo.rb");
        assert_eq!(*index.uri_pool.get(&uri_2).unwrap(), "file:///foo2.rb");
    }
}
