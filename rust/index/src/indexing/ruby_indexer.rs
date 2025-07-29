//! Visit the Ruby AST and create the definitions.

use std::collections::HashMap;

use crate::model::definitions::{ClassDefinition, Definition, ModuleDefinition};
use crate::model::ids::{DeclarationId, UriId};
use crate::offset::Offset;

use ruby_prism::Visit;

// A collection of definitions specific for this indexer. This is not the global representation of declarations, just a
// list of all definitions discovered that will be merged into the global index. It helps us avoid having to hash fully
// qualified names into DeclarationIds multiple times
pub struct DefinitionCollection {
    pub node_id: DeclarationId,
    pub definitions: Vec<Definition>,
}

impl DefinitionCollection {
    #[must_use]
    pub fn new(node_id: DeclarationId) -> Self {
        Self {
            node_id,
            definitions: Vec::new(),
        }
    }

    pub fn add_definition(&mut self, definition: Definition) {
        self.definitions.push(definition);
    }
}

/// The indexer for the definitions found in the Ruby source code.
///
/// It implements the `Visit` trait from `ruby_prism` to visit the AST and create a hash of definitions that must be
/// merged into the global state later.
///
/// Example:
///
/// ```
/// use index::indexing::ruby_indexer::RubyIndexer;
/// use index::model::index::Index;
///
/// let mut index = Index::new();
/// let uri_id = index.intern_uri("file:///path/to/file.rb".to_string());
/// let mut indexer = RubyIndexer::new();
///
/// let source = "class Foo; end";
/// indexer.index(uri_id, &source);
///
/// assert_eq!(indexer.definitions.len(), 1);
/// ```
pub struct RubyIndexer {
    uri_id: Option<UriId>,
    pub definitions: HashMap<String, DefinitionCollection>,
    nesting_stacks: Vec<Vec<String>>,
}

impl RubyIndexer {
    #[must_use]
    pub fn new() -> Self {
        Self {
            uri_id: None,
            definitions: HashMap::new(),
            nesting_stacks: vec![Vec::new()],
        }
    }

    pub fn index(&mut self, uri_id: UriId, source: &str) {
        self.uri_id = Some(uri_id);
        let result = ruby_prism::parse(source.as_ref());
        self.visit(&result.node());
        self.uri_id = None;
    }

    fn location_to_string(location: &ruby_prism::Location) -> String {
        String::from_utf8_lossy(location.as_slice()).to_string()
    }

    fn location_to_offset(&self, location: &ruby_prism::Location) -> Offset {
        Offset::new(
            self.uri_id.expect("URI must be set during indexing"),
            location
                .start_offset()
                .try_into()
                .expect("Expected usize `start_offset` to fit in `u32`"),
            location
                .end_offset()
                .try_into()
                .expect("Expected usize `end_offset` to fit in `u32`"),
        )
    }

    fn index_definition(&mut self, name: String, definition: Definition) {
        // If the entry already exists in the hashmap
        if self.definitions.contains_key(&name) {
            self.definitions
                .get_mut(&name)
                .expect("Definition should exist")
                .add_definition(definition);

            return;
        }

        // If the entry does not exist, take the slow path and create a new one
        let node_id = DeclarationId::new(&name);
        let mut file_definitions = DefinitionCollection::new(node_id);
        file_definitions.add_definition(definition);
        self.definitions.insert(name, file_definitions);
    }

    fn with_updated_nesting<F>(&mut self, name: &str, perform_visit: F)
    where
        F: FnOnce(&mut Self, String),
    {
        if name.starts_with("::") {
            let trimmed = name.trim_start_matches("::").to_string();
            self.nesting_stacks.push(vec![trimmed]);
        } else {
            let current_stack: &mut Vec<String> = self
                .nesting_stacks
                .last_mut()
                .expect("There should always be at least one stack. This is a bug");

            current_stack.push(name.to_string());
        }

        let current_stack = self
            .nesting_stacks
            .last()
            .expect("There should always be at least one stack. This is a bug");

        perform_visit(self, current_stack.join("::"));

        if name.starts_with("::") {
            self.nesting_stacks.pop();
        } else {
            let current_stack: &mut Vec<String> = self
                .nesting_stacks
                .last_mut()
                .expect("There should always be at least one stack. This is a bug");

            current_stack.pop();
        }
    }
}

impl Default for RubyIndexer {
    fn default() -> Self {
        Self::new()
    }
}

impl Visit<'_> for RubyIndexer {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'_>) {
        let name = Self::location_to_string(&node.constant_path().location());

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let definition = Definition::Class(Box::new(ClassDefinition::new(
                indexer.location_to_offset(&node.location()),
            )));
            indexer.index_definition(fully_qualified_name, definition);

            if let Some(body) = node.body() {
                indexer.visit(&body);
            }
        });
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode) {
        let name = Self::location_to_string(&node.constant_path().location());

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let definition = Definition::Module(Box::new(ModuleDefinition::new(
                indexer.location_to_offset(&node.location()),
            )));
            indexer.index_definition(fully_qualified_name, definition);

            if let Some(body) = node.body() {
                indexer.visit(&body);
            }
        });
    }
}
