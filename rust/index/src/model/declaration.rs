//! Represent the declaration of a name in the Ruby model.
//!
//! A declaration is composed of one or more `Definition`s.
//!
//! Let's consider the following example:
//! ```ruby
//! class Foo; end
//! def Foo; end
//! ```
//!
//! There are 2 definitions:
//! 1. The class definition for `Foo`
//! 2. The method definition for `Foo`
//!
//! and only one declaration: for the name `Foo`

use std::collections::HashMap;

use crate::model::{definitions::Definition, ids::UriId};

#[derive(Debug)]
pub struct Declaration {
    name: String,
    definitions: HashMap<UriId, Vec<Definition>>,
}

impl Declaration {
    #[must_use]
    pub fn new(name: String) -> Self {
        Self {
            name,
            definitions: HashMap::new(),
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.definitions.is_empty()
    }

    // Deletes all definitions for the given URI
    pub fn delete_definitions(&mut self, uri_id: UriId) {
        self.definitions.remove(&uri_id);
    }

    // Updates the definitions for the given URI. Currently, only overrides with whatever new definitions were
    // discovered
    pub fn update_definitions(&mut self, uri_id: UriId, definitions: Vec<Definition>) {
        self.definitions.insert(uri_id, definitions);
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }
}

// Unit tests for Declaration::delete_definitions and is_empty
#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::definitions::{ClassDefinition, ModuleDefinition};
    use crate::offset::Offset;

    #[test]
    fn delete_definitions_only_removes_for_given_uri() {
        let mut decl = Declaration::new("Foo".to_string());
        let uri1 = UriId::new("file1");
        let uri2 = UriId::new("file2");

        let defs1 = vec![Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 10))))];
        let defs2 = vec![Definition::Module(Box::new(ModuleDefinition::new(Offset::new(20, 30))))];
        decl.update_definitions(uri1, defs1);
        decl.update_definitions(uri2, defs2);

        decl.delete_definitions(uri1);
        assert!(
            !decl.is_empty(),
            "Declaration should not be empty after deleting only one URI"
        );

        decl.delete_definitions(uri2);
        assert!(decl.is_empty(), "Declaration should be empty after deleting all URIs");
    }

    #[test]
    fn updating_with_brand_new_definitions() {
        let mut decl = Declaration::new("Foo".to_string());
        let uri = UriId::new("file1");

        let defs = vec![Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 10))))];
        decl.update_definitions(uri, defs);

        assert!(!decl.is_empty());
        assert_eq!(decl.definitions.get(&uri).unwrap().len(), 1);
    }

    #[test]
    fn updating_existing_definitions() {
        let mut decl = Declaration::new("Foo".to_string());
        let uri = UriId::new("file1");

        let defs = vec![Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 10))))];
        decl.update_definitions(uri, defs);

        let new_defs = vec![Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 20))))];
        decl.update_definitions(uri, new_defs);

        let inserted_def = decl.definitions.get(&uri).unwrap();
        assert_eq!(inserted_def.len(), 1);
    }

    #[test]
    fn updating_existing_definitions_with_fewer_entries() {
        // Example: the same class define twice in the same file. The user then removes one of the definitions,
        // effectively reducing the vector of definitions for that URI from 2 to 1
        let mut decl = Declaration::new("Foo".to_string());
        let uri = UriId::new("file1");

        let defs = vec![
            Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 10)))),
            Definition::Class(Box::new(ClassDefinition::new(Offset::new(50, 80)))),
        ];
        decl.update_definitions(uri, defs);
        assert_eq!(decl.definitions.len(), 1);
        assert_eq!(decl.definitions.get(&uri).unwrap().len(), 2);

        let new_defs = vec![Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 20))))];
        decl.update_definitions(uri, new_defs);

        let inserted_def = decl.definitions.get(&uri).unwrap();
        assert_eq!(inserted_def.len(), 1);
        assert_eq!(decl.definitions.len(), 1);
    }

    #[test]
    fn updates_only_impact_related_uri() {
        let mut decl = Declaration::new("Foo".to_string());

        let uri_1 = UriId::new("file:///foo.rb");
        let defs_1 = vec![Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 10))))];
        let uri_2 = UriId::new("file:///bar.rb");
        let defs_2 = vec![Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 10))))];

        decl.update_definitions(uri_1, defs_1);
        decl.update_definitions(uri_2, defs_2);
        assert_eq!(decl.definitions.len(), 2);

        let new_defs = vec![Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 20))))];
        decl.update_definitions(uri_1, new_defs);

        let inserted_def = decl.definitions.get(&uri_2).unwrap();
        assert_eq!(*inserted_def.first().unwrap().offset(), Offset::new(0, 10));
        assert_eq!(decl.definitions.len(), 2);
    }
}
