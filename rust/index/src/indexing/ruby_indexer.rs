//! Visit the Ruby AST and create the definitions.

use std::sync::Arc;

use crate::indexing::errors::IndexingError;
use crate::model::definitions::{ClassDefinition, Definition, ModuleDefinition};
use crate::model::graph::Graph;
use crate::model::ids::UriId;
use crate::offset::Offset;

use ruby_prism::Visit;

/// The indexer for the definitions found in the Ruby source code.
///
/// It implements the `Visit` trait from `ruby_prism` to visit the AST and create a hash of definitions that must be
/// merged into the global state later.
pub struct RubyIndexer {
    uri_id: UriId,
    nesting_stacks: Vec<Vec<String>>,
    errors: Vec<IndexingError>,
    graph: Arc<Graph>,
}

impl RubyIndexer {
    #[must_use]
    pub fn new(graph: Arc<Graph>, uri: String) -> Self {
        let uri_id = graph.add_uri(uri);
        graph.remove_definitions_for_uri(uri_id);

        Self {
            uri_id,
            nesting_stacks: vec![Vec::new()],
            graph,
            errors: Vec::new(),
        }
    }

    #[must_use]
    pub fn into_errors(self) -> Vec<IndexingError> {
        self.errors
    }

    pub fn add_error(&mut self, error: IndexingError) {
        self.errors.push(error);
    }

    pub fn index(&mut self, source: &str) {
        let result = ruby_prism::parse(source.as_ref());
        self.visit(&result.node());
    }

    fn location_to_string(location: &ruby_prism::Location) -> String {
        String::from_utf8_lossy(location.as_slice()).to_string()
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

impl Visit<'_> for RubyIndexer {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'_>) {
        let name = Self::location_to_string(&node.constant_path().location());

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let definition = Definition::Class(Box::new(ClassDefinition::new(Offset::from_prism_location(
                &node.location(),
            ))));

            indexer
                .graph
                .add_definition(indexer.uri_id, fully_qualified_name, definition);

            if let Some(body) = node.body() {
                indexer.visit(&body);
            }
        });
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode) {
        let name = Self::location_to_string(&node.constant_path().location());

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let definition = Definition::Module(Box::new(ModuleDefinition::new(Offset::from_prism_location(
                &node.location(),
            ))));
            indexer
                .graph
                .add_definition(indexer.uri_id, fully_qualified_name, definition);

            if let Some(body) = node.body() {
                indexer.visit(&body);
            }
        });
    }
}
