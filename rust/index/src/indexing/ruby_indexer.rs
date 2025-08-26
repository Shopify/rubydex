//! Visit the Ruby AST and create the definitions.

use crate::indexing::errors::IndexingError;
use crate::model::definitions::{
    AttrAccessorDefinition, AttrReaderDefinition, AttrWriterDefinition, ClassDefinition, ClassVariableDefinition,
    ConstantDefinition, Definition, GlobalVariableDefinition, InstanceVariableDefinition, MethodDefinition,
    ModuleDefinition, Parameter, ParameterKind,
};
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
                ruby_prism::Node::GlobalVariableTargetNode { .. } => {
                    let name = Self::location_to_string(&left.location());

                    let definition = Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                        Offset::from_prism_location(&left.location()),
                    )));

                    self.local_index.add_definition(self.uri_id, name, definition);
                }
                ruby_prism::Node::InstanceVariableTargetNode { .. } => {
                    let name = Self::location_to_string(&left.location());

                    self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
                        let definition = Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
                            Offset::from_prism_location(&left.location()),
                        )));

                        indexer
                            .local_index
                            .add_definition(indexer.uri_id, fully_qualified_name, definition);
                    });
                }
                ruby_prism::Node::ClassVariableTargetNode { .. } => {
                    let name = Self::location_to_string(&left.location());

                    self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
                        let definition = Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
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

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode) {
        let name = Self::location_to_string(&node.name_loc());

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let mut parameters: Vec<Parameter> = Vec::new();
            if let Some(parameters_list) = node.parameters() {
                for parameter in parameters_list.requireds().iter() {
                    parameters.push(Parameter::new(
                        Offset::from_prism_location(&parameter.location()),
                        Self::location_to_string(&parameter.location()),
                        ParameterKind::RequiredPositional,
                    ));
                }
                for parameter in parameters_list.optionals().iter() {
                    if let ruby_prism::Node::OptionalParameterNode { .. } = parameter
                        && let Some(opt_param) = parameter.as_optional_parameter_node()
                    {
                        parameters.push(Parameter::new(
                            Offset::from_prism_location(&parameter.location()),
                            Self::location_to_string(&opt_param.name_loc()),
                            ParameterKind::OptionalPositional,
                        ));
                    }
                }
                if let Some(rest) = parameters_list.rest()
                    && let Some(rest_param) = rest.as_rest_parameter_node()
                {
                    parameters.push(Parameter::new(
                        Offset::from_prism_location(&rest.location()),
                        Self::location_to_string(&rest_param.name_loc().unwrap_or_else(|| rest.location())),
                        ParameterKind::RestPositional,
                    ));
                }
                for post in parameters_list.posts().iter() {
                    parameters.push(Parameter::new(
                        Offset::from_prism_location(&post.location()),
                        Self::location_to_string(&post.location()),
                        ParameterKind::Post,
                    ));
                }
                for keyword in parameters_list.keywords().iter() {
                    match keyword {
                        ruby_prism::Node::RequiredKeywordParameterNode { .. } => {
                            if let Some(required) = keyword.as_required_keyword_parameter_node() {
                                parameters.push(Parameter::new(
                                    Offset::from_prism_location(&keyword.location()),
                                    Self::location_to_string(&required.name_loc())
                                        .trim_end_matches(':')
                                        .to_string(),
                                    ParameterKind::RequiredKeyword,
                                ));
                            }
                        }
                        ruby_prism::Node::OptionalKeywordParameterNode { .. } => {
                            if let Some(optional) = keyword.as_optional_keyword_parameter_node() {
                                parameters.push(Parameter::new(
                                    Offset::from_prism_location(&keyword.location()),
                                    Self::location_to_string(&optional.name_loc())
                                        .trim_end_matches(':')
                                        .to_string(),
                                    ParameterKind::OptionalKeyword,
                                ));
                            }
                        }
                        _ => {}
                    }
                }
                if let Some(rest) = parameters_list.keyword_rest()
                    && let Some(rest_param) = rest.as_keyword_rest_parameter_node()
                {
                    parameters.push(Parameter::new(
                        Offset::from_prism_location(&rest.location()),
                        Self::location_to_string(&rest_param.name_loc().unwrap_or_else(|| rest.location())),
                        ParameterKind::RestKeyword,
                    ));
                }
                if let Some(block) = parameters_list.block() {
                    parameters.push(Parameter::new(
                        Offset::from_prism_location(&block.location()),
                        Self::location_to_string(&block.name_loc().unwrap_or_else(|| block.location())),
                        ParameterKind::Block,
                    ));
                }
            }

            let method = MethodDefinition::new(
                Offset::from_prism_location(&node.location()),
                parameters,
                node.receiver()
                    .is_some_and(|receiver| receiver.as_self_node().is_some()),
            );

            indexer.local_index.add_definition(
                indexer.uri_id,
                fully_qualified_name,
                Definition::Method(Box::new(method)),
            );
        });

        if let Some(body) = node.body() {
            self.visit(&body);
        }
    }

    fn visit_call_node(&mut self, node: &ruby_prism::CallNode) {
        let message_loc = node.message_loc();

        if message_loc.is_none() {
            return;
        }

        let message = Self::location_to_string(&message_loc.unwrap());
        if message != "attr_accessor" && message != "attr_reader" && message != "attr_writer" {
            return;
        }

        if node
            .receiver()
            .is_some_and(|receiver| receiver.as_self_node().is_none())
        {
            // We don't index `attr_` calls not made on `self`
            return;
        }

        let arguments = node.arguments();
        if let Some(arguments) = arguments {
            for argument in arguments.arguments().iter() {
                if let ruby_prism::Node::SymbolNode { .. } = argument {
                    let symbol = argument.as_symbol_node().unwrap();
                    let name_with_colon = Self::location_to_string(&symbol.location());
                    let name = name_with_colon.trim_start_matches(':');

                    self.with_updated_nesting(name, |indexer, fully_qualified_name| match message.as_str() {
                        "attr_accessor" => {
                            indexer.local_index.add_definition(
                                indexer.uri_id,
                                fully_qualified_name.clone(),
                                Definition::AttrAccessor(Box::new(AttrAccessorDefinition::new(
                                    Offset::from_prism_location(&symbol.location()),
                                ))),
                            );

                            indexer.local_index.add_definition(
                                indexer.uri_id,
                                format!("{fully_qualified_name}="),
                                Definition::AttrAccessor(Box::new(AttrAccessorDefinition::new(
                                    Offset::from_prism_location(&symbol.location()),
                                ))),
                            );
                        }
                        "attr_reader" => {
                            indexer.local_index.add_definition(
                                indexer.uri_id,
                                fully_qualified_name,
                                Definition::AttrReader(Box::new(AttrReaderDefinition::new(
                                    Offset::from_prism_location(&symbol.location()),
                                ))),
                            );
                        }
                        "attr_writer" => {
                            indexer.local_index.add_definition(
                                indexer.uri_id,
                                format!("{fully_qualified_name}="),
                                Definition::AttrWriter(Box::new(AttrWriterDefinition::new(
                                    Offset::from_prism_location(&symbol.location()),
                                ))),
                            );
                        }
                        _ => panic!("Unexpected message: {message}"),
                    });
                }
            }
        }
    }

    fn visit_global_variable_write_node(&mut self, node: &ruby_prism::GlobalVariableWriteNode) {
        let name = Self::location_to_string(&node.name_loc());

        let definition = Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
            Offset::from_prism_location(&node.name_loc()),
        )));

        self.local_index.add_definition(self.uri_id, name, definition);

        self.visit(&node.value());
    }

    fn visit_instance_variable_write_node(&mut self, node: &ruby_prism::InstanceVariableWriteNode) {
        let name = Self::location_to_string(&node.name_loc());

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let definition = Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
                Offset::from_prism_location(&node.name_loc()),
            )));

            indexer
                .local_index
                .add_definition(indexer.uri_id, fully_qualified_name, definition);
        });

        self.visit(&node.value());
    }

    fn visit_class_variable_write_node(&mut self, node: &ruby_prism::ClassVariableWriteNode) {
        let name = Self::location_to_string(&node.name_loc());

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let definition = Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
                Offset::from_prism_location(&node.name_loc()),
            )));

            indexer
                .local_index
                .add_definition(indexer.uri_id, fully_qualified_name, definition);
        });

        self.visit(&node.value());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
        assert_eq!(definitions[0].start(), 0);
        assert_eq!(definitions[0].end(), 50);

        let definitions = context.graph.get("Foo::Bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 12);
        assert_eq!(definitions[0].end(), 46);

        let definitions = context.graph.get("Foo::Bar::Baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 26);
        assert_eq!(definitions[0].end(), 40);

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
        assert_eq!(definitions[0].start(), 0);
        assert_eq!(definitions[0].end(), 53);

        let definitions = context.graph.get("Foo::Bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 13);
        assert_eq!(definitions[0].end(), 49);

        let definitions = context.graph.get("Foo::Bar::Baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 28);
        assert_eq!(definitions[0].end(), 43);

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
        assert_eq!(definitions[0].start(), 0);
        assert_eq!(definitions[0].end(), 3);

        let definitions = context.graph.get("Foo::FOO").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 21);
        assert_eq!(definitions[0].end(), 24);
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
        assert_eq!(definitions[0].start(), 0);
        assert_eq!(definitions[0].end(), 8);

        let definitions = context.graph.get("Foo::FOO::BAR").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 26);
        assert_eq!(definitions[0].end(), 34);

        let definitions = context.graph.get("BAZ").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 41);
        assert_eq!(definitions[0].end(), 46);
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
        assert_eq!(definitions[0].start(), 0);
        assert_eq!(definitions[0].end(), 3);

        let definitions = context.graph.get("BAR::BAZ").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 5);
        assert_eq!(definitions[0].end(), 13);

        let definitions = context.graph.get("Foo::FOO").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 34);
        assert_eq!(definitions[0].end(), 37);

        let definitions = context.graph.get("Foo::BAR::BAZ").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 39);
        assert_eq!(definitions[0].end(), 47);

        let definitions = context.graph.get("BAZ").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 49);
        assert_eq!(definitions[0].end(), 54);
    }

    #[test]
    fn index_def_node() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            def foo; end

            class Foo
              def bar; end
              def self.baz; end
            end
            "
        });

        let definitions = context.graph.get("foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 0);
        assert_eq!(definitions[0].end(), 12);

        match definitions[0] {
            Definition::Method(it) => {
                assert_eq!(it.parameters().len(), 0);
                assert!(!it.is_singleton());
            }
            _ => panic!("Expected method definition"),
        }

        let definitions = context.graph.get("Foo::bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 26);
        assert_eq!(definitions[0].end(), 38);

        match definitions[0] {
            Definition::Method(it) => {
                assert_eq!(it.parameters().len(), 0);
                assert!(!it.is_singleton());
            }
            _ => panic!("Expected method definition"),
        }

        let definitions = context.graph.get("Foo::baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 41);
        assert_eq!(definitions[0].end(), 58);

        match definitions[0] {
            Definition::Method(it) => {
                assert_eq!(it.parameters().len(), 0);
                assert!(it.is_singleton());
            }
            _ => panic!("Expected method definition"),
        }
    }

    #[test]
    fn index_def_node_with_parameters() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            def foo(a, b = 42, *c, d, e:, g: 42, **i, &j); end
            "
        });

        let definitions = context.graph.get("foo").unwrap();

        match definitions[0] {
            Definition::Method(it) => {
                assert_eq!(it.parameters().len(), 8);

                assert_eq!(it.parameters()[0].name, "a");
                assert_eq!(it.parameters()[0].kind, ParameterKind::RequiredPositional);

                assert_eq!(it.parameters()[1].name, "b");
                assert_eq!(it.parameters()[1].kind, ParameterKind::OptionalPositional);

                assert_eq!(it.parameters()[2].name, "c");
                assert_eq!(it.parameters()[2].kind, ParameterKind::RestPositional);

                assert_eq!(it.parameters()[3].name, "d");
                assert_eq!(it.parameters()[3].kind, ParameterKind::Post);

                assert_eq!(it.parameters()[4].name, "e");
                assert_eq!(it.parameters()[4].kind, ParameterKind::RequiredKeyword);

                assert_eq!(it.parameters()[5].name, "g");
                assert_eq!(it.parameters()[5].kind, ParameterKind::OptionalKeyword);

                assert_eq!(it.parameters()[6].name, "i");
                assert_eq!(it.parameters()[6].kind, ParameterKind::RestKeyword);

                assert_eq!(it.parameters()[7].name, "j");
                assert_eq!(it.parameters()[7].kind, ParameterKind::Block);
            }
            _ => panic!("Expected method definition"),
        }
    }

    #[test]
    fn index_attr_accessor_definition() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            attr_accessor :foo

            class Foo
              attr_accessor :bar, :baz
            end
            "
        });

        let definitions = context.graph.get("foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 14);
        assert_eq!(definitions[0].end(), 18);

        let definitions = context.graph.get("foo=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 14);
        assert_eq!(definitions[0].end(), 18);

        let definitions = context.graph.get("Foo::bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 46);
        assert_eq!(definitions[0].end(), 50);

        let definitions = context.graph.get("Foo::bar=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 46);
        assert_eq!(definitions[0].end(), 50);

        let definitions = context.graph.get("Foo::baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 52);
        assert_eq!(definitions[0].end(), 56);

        let definitions = context.graph.get("Foo::baz=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 52);
        assert_eq!(definitions[0].end(), 56);
    }

    #[test]
    fn index_attr_reader_definition() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            attr_reader :foo

            class Foo
              attr_reader :bar, :baz
            end
            "
        });

        let definitions = context.graph.get("foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 12);
        assert_eq!(definitions[0].end(), 16);

        assert!(context.graph.get("foo=").is_none());

        let definitions = context.graph.get("Foo::bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 42);
        assert_eq!(definitions[0].end(), 46);

        assert!(context.graph.get("Foo::bar=").is_none());

        let definitions = context.graph.get("Foo::baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 48);
        assert_eq!(definitions[0].end(), 52);

        assert!(context.graph.get("Foo::baz=").is_none());
    }

    #[test]
    fn index_attr_writer_definition() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            attr_writer :foo

            class Foo
              attr_writer :bar, :baz
            end
            "
        });

        let definitions = context.graph.get("foo=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 12);
        assert_eq!(definitions[0].end(), 16);

        assert!(context.graph.get("foo").is_none());

        let definitions = context.graph.get("Foo::bar=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 42);
        assert_eq!(definitions[0].end(), 46);

        assert!(context.graph.get("Foo::bar").is_none());

        let definitions = context.graph.get("Foo::baz=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 48);
        assert_eq!(definitions[0].end(), 52);

        assert!(context.graph.get("Foo::baz").is_none());
    }

    #[test]
    fn index_global_variable_definition() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            $foo = 1
            $bar, $baz = 2, 3

            class Foo
              $qux = 2
            end
            "
        });

        let definitions = context.graph.get("$foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 0);
        assert_eq!(definitions[0].end(), 4);

        let definitions = context.graph.get("$bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 9);
        assert_eq!(definitions[0].end(), 13);

        let definitions = context.graph.get("$baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 15);
        assert_eq!(definitions[0].end(), 19);

        let definitions = context.graph.get("$qux").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 40);
        assert_eq!(definitions[0].end(), 44);
    }

    #[test]
    fn index_instance_variable_definition() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            @foo = 1

            class Foo
              @bar = 2

              @baz, @qux = 3, 4
            end
            "
        });

        let definitions = context.graph.get("@foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 0);
        assert_eq!(definitions[0].end(), 4);

        let definitions = context.graph.get("Foo::@bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 22);
        assert_eq!(definitions[0].end(), 26);

        let definitions = context.graph.get("Foo::@baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 34);
        assert_eq!(definitions[0].end(), 38);

        let definitions = context.graph.get("Foo::@qux").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 40);
        assert_eq!(definitions[0].end(), 44);
    }

    #[test]
    fn index_class_variable_definition() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            @@foo = 1

            class Foo
              @@bar = 2

              @@baz, @@qux = 3, 4
            end
            "
        });

        let definitions = context.graph.get("@@foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 0);
        assert_eq!(definitions[0].end(), 5);

        let definitions = context.graph.get("Foo::@@bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 23);
        assert_eq!(definitions[0].end(), 28);

        let definitions = context.graph.get("Foo::@@baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 36);
        assert_eq!(definitions[0].end(), 41);

        let definitions = context.graph.get("Foo::@@qux").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 43);
        assert_eq!(definitions[0].end(), 48);
    }
}
