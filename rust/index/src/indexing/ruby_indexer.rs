//! Visit the Ruby AST and create the definitions.

use crate::indexing::errors::IndexingError;
use crate::indexing::indexed_data::IndexingThreadData;
use crate::model::definitions::{ClassDefinition, Definition, ModuleDefinition};
use crate::model::ids::UriId;
use crate::offset::Offset;

use ruby_prism::Visit;

/// The indexer for the definitions found in the Ruby source code.
///
/// It implements the `Visit` trait from `ruby_prism` to visit the AST and create a hash of definitions that must be
/// merged into the global state later.
pub struct RubyIndexer {
    uri_id: Option<UriId>,
    indexed_data: IndexingThreadData,
    nesting_stacks: Vec<Vec<String>>,
    errors: Vec<IndexingError>,
}

impl RubyIndexer {
    #[must_use]
    pub fn new() -> Self {
        Self {
            uri_id: None,
            nesting_stacks: vec![Vec::new()],
            indexed_data: IndexingThreadData::new(),
            errors: Vec::new(),
        }
    }

    // Returns the information collected by this indexer as owned data, so that it can be moved into the global index
    #[must_use]
    pub fn into(self) -> (IndexingThreadData, Vec<IndexingError>) {
        (self.indexed_data, self.errors)
    }

    pub fn add_error(&mut self, error: IndexingError) {
        self.errors.push(error);
    }

    pub fn index(&mut self, uri: String, source: &str) {
        self.uri_id = Some(self.indexed_data.add_uri(uri));
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
        self.indexed_data.add_definition(name, definition);
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
