//! Visit the Ruby AST and create the definitions.

use crate::indexing::errors::IndexingError;
use crate::model::definitions::{ClassDefinition, ConstantDefinition, Definition, ModuleDefinition};
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
    local_index: Graph,
    nesting_stacks: Vec<Vec<String>>,
    errors: Vec<IndexingError>,
}

impl RubyIndexer {
    #[must_use]
    pub fn new(uri: String) -> Self {
        let mut local_index = Graph::new();
        let uri_id = local_index.add_uri(uri);

        Self {
            uri_id,
            nesting_stacks: vec![Vec::new()],
            local_index,
            errors: Vec::new(),
        }
    }

    #[must_use]
    pub fn into_parts(self) -> (Graph, Vec<IndexingError>) {
        (self.local_index, self.errors)
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
                .local_index
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
                .local_index
                .add_definition(indexer.uri_id, fully_qualified_name, definition);

            if let Some(body) = node.body() {
                indexer.visit(&body);
            }
        });
    }

    fn visit_constant_write_node(&mut self, node: &ruby_prism::ConstantWriteNode) {
        let name = Self::location_to_string(&node.name_loc());

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let definition = Definition::Constant(Box::new(ConstantDefinition::new(Offset::from_prism_location(
                &node.name_loc(),
            ))));

            indexer
                .local_index
                .add_definition(indexer.uri_id, fully_qualified_name, definition);
        });

        self.visit(&node.value());
    }

    fn visit_constant_path_write_node(&mut self, node: &ruby_prism::ConstantPathWriteNode) {
        let name = Self::location_to_string(&node.target().location());

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let definition = Definition::Constant(Box::new(ConstantDefinition::new(Offset::from_prism_location(
                &node.target().location(),
            ))));

            indexer
                .local_index
                .add_definition(indexer.uri_id, fully_qualified_name, definition);
        });

        self.visit(&node.value());
    }

    fn visit_multi_write_node(&mut self, node: &ruby_prism::MultiWriteNode) {
        for left in node.lefts().iter() {
            match left {
                ruby_prism::Node::ConstantTargetNode { .. } | ruby_prism::Node::ConstantPathTargetNode { .. } => {
                    let name = Self::location_to_string(&left.location());

                    self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
                        let definition = Definition::Constant(Box::new(ConstantDefinition::new(
                            Offset::from_prism_location(&left.location()),
                        )));

                        indexer
                            .local_index
                            .add_definition(indexer.uri_id, fully_qualified_name, definition);
                    });
                }
                _ => {}
            }
        }

        self.visit(&node.value());
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::GraphTest;

    #[test]
    fn index_class_node() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            class Foo
              class Bar
                class Baz; end
              end
            end
            "
        });

        let definitions = context.graph.get("Foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 0);
        assert_eq!(definitions[0].end_offset(), 50);

        let definitions = context.graph.get("Foo::Bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 12);
        assert_eq!(definitions[0].end_offset(), 46);

        let definitions = context.graph.get("Foo::Bar::Baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 26);
        assert_eq!(definitions[0].end_offset(), 40);

        let not_found = context.graph.get("Foo::Bar::Baz::Qux");
        assert!(not_found.is_none());
    }

    #[test]
    fn index_class_node_with_qualified_name() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            class Foo::Bar
              class Baz::Qux; end
              class ::Quuux; end
            end
            "
        });

        assert_eq!(context.graph.get("Foo::Bar").unwrap().len(), 1);
        assert_eq!(context.graph.get("Foo::Bar::Baz::Qux").unwrap().len(), 1);
        assert!(context.graph.get("Foo::Bar::Baz::Qux::Quuux").is_none());
        assert_eq!(context.graph.get("Quuux").unwrap().len(), 1);
    }

    #[test]
    fn index_module_node() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            module Foo
              module Bar
                module Baz; end
              end
            end
            "
        });

        let definitions = context.graph.get("Foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 0);
        assert_eq!(definitions[0].end_offset(), 53);

        let definitions = context.graph.get("Foo::Bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 13);
        assert_eq!(definitions[0].end_offset(), 49);

        let definitions = context.graph.get("Foo::Bar::Baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 28);
        assert_eq!(definitions[0].end_offset(), 43);

        let not_found = context.graph.get("Foo::Bar::Baz::Qux");
        assert!(not_found.is_none());
    }

    #[test]
    fn index_module_node_with_qualified_name() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            module Foo::Bar
              module Baz::Qux; end
              module ::Quuux; end
            end
            "
        });

        assert_eq!(context.graph.get("Foo::Bar").unwrap().len(), 1);
        assert_eq!(context.graph.get("Foo::Bar::Baz::Qux").unwrap().len(), 1);
        assert!(context.graph.get("Foo::Bar::Baz::Qux::Quuux").is_none());
        assert_eq!(context.graph.get("Quuux").unwrap().len(), 1);
    }

    #[test]
    fn index_constant_write_node() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            FOO = 1

            class Foo
              FOO = 2
            end
            "
        });

        let definitions = context.graph.get("FOO").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 0);
        assert_eq!(definitions[0].end_offset(), 3);

        let definitions = context.graph.get("Foo::FOO").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 21);
        assert_eq!(definitions[0].end_offset(), 24);
    }

    #[test]
    fn index_constant_path_write_node() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            FOO::BAR = 1

            class Foo
              FOO::BAR = 2
              ::BAZ = 3
            end
            "
        });

        let definitions = context.graph.get("FOO::BAR").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 0);
        assert_eq!(definitions[0].end_offset(), 8);

        let definitions = context.graph.get("Foo::FOO::BAR").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 26);
        assert_eq!(definitions[0].end_offset(), 34);

        let definitions = context.graph.get("BAZ").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 41);
        assert_eq!(definitions[0].end_offset(), 46);
    }

    #[test]
    fn index_constant_multi_write_node() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            FOO, BAR::BAZ = 1, 2

            class Foo
              FOO, BAR::BAZ, ::BAZ = 3, 4, 5
            end
            "
        });

        let definitions = context.graph.get("FOO").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 0);
        assert_eq!(definitions[0].end_offset(), 3);

        let definitions = context.graph.get("BAR::BAZ").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 5);
        assert_eq!(definitions[0].end_offset(), 13);

        let definitions = context.graph.get("Foo::FOO").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 34);
        assert_eq!(definitions[0].end_offset(), 37);

        let definitions = context.graph.get("Foo::BAR::BAZ").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 39);
        assert_eq!(definitions[0].end_offset(), 47);

        let definitions = context.graph.get("BAZ").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 49);
        assert_eq!(definitions[0].end_offset(), 54);
    }
}
