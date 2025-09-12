//! Visit the Ruby AST and create the definitions.

use crate::indexing::errors::IndexingError;
use crate::model::definitions::{
    AttrAccessorDefinition, AttrReaderDefinition, AttrWriterDefinition, ClassDefinition, ClassVariableDefinition,
    ConstantDefinition, Definition, GlobalVariableDefinition, InstanceVariableDefinition, MethodDefinition,
    ModuleDefinition, Parameter, ParameterStruct,
};
use crate::model::graph::Graph;
use crate::model::ids::{NameId, UriId};
use crate::offset::Offset;
use crate::source_location::SourceLocationConverter;

use ruby_prism::Visit;

pub type IndexerParts = (Graph, Vec<IndexingError>);

/// The indexer for the definitions found in the Ruby source code.
///
/// It implements the `Visit` trait from `ruby_prism` to visit the AST and create a hash of definitions that must be
/// merged into the global state later.
pub struct RubyIndexer<'a> {
    uri_id: UriId,
    local_index: Graph,
    nesting_stacks: Vec<Vec<String>>,
    errors: Vec<IndexingError>,
    location_converter: &'a dyn SourceLocationConverter,
}

impl<'a> RubyIndexer<'a> {
    #[must_use]
    pub fn new(uri: String, location_converter: &'a dyn SourceLocationConverter) -> Self {
        let mut local_index = Graph::new();
        let uri_id = local_index.add_uri(uri);

        Self {
            uri_id,
            nesting_stacks: vec![Vec::new()],
            local_index,
            errors: Vec::new(),
            location_converter,
        }
    }

    #[must_use]
    pub fn into_parts(self) -> IndexerParts {
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

    fn collect_parameters(node: &ruby_prism::DefNode) -> Vec<Parameter> {
        let mut parameters: Vec<Parameter> = Vec::new();

        if let Some(parameters_list) = node.parameters() {
            for parameter in parameters_list.requireds().iter() {
                let location = parameter.location();

                parameters.push(Parameter::RequiredPositional(ParameterStruct::new(
                    Offset::from_prism_location(&location),
                    Self::location_to_string(&location),
                )));
            }

            for parameter in parameters_list.optionals().iter() {
                let opt_param = parameter.as_optional_parameter_node().unwrap();
                let name_loc = opt_param.name_loc();

                parameters.push(Parameter::OptionalPositional(ParameterStruct::new(
                    Offset::from_prism_location(&name_loc),
                    Self::location_to_string(&name_loc),
                )));
            }

            if let Some(rest) = parameters_list.rest() {
                let rest_param = rest.as_rest_parameter_node().unwrap();
                let location = rest_param.name_loc().unwrap_or_else(|| rest.location());

                parameters.push(Parameter::RestPositional(ParameterStruct::new(
                    Offset::from_prism_location(&location),
                    Self::location_to_string(&location),
                )));
            }

            for post in parameters_list.posts().iter() {
                let location = post.location();

                parameters.push(Parameter::Post(ParameterStruct::new(
                    Offset::from_prism_location(&location),
                    Self::location_to_string(&location),
                )));
            }

            for keyword in parameters_list.keywords().iter() {
                match keyword {
                    ruby_prism::Node::RequiredKeywordParameterNode { .. } => {
                        let required = keyword.as_required_keyword_parameter_node().unwrap();
                        let name_loc = required.name_loc();

                        parameters.push(Parameter::RequiredKeyword(ParameterStruct::new(
                            Offset::from_prism_location(&name_loc),
                            Self::location_to_string(&name_loc).trim_end_matches(':').to_string(),
                        )));
                    }
                    ruby_prism::Node::OptionalKeywordParameterNode { .. } => {
                        let optional = keyword.as_optional_keyword_parameter_node().unwrap();
                        let name_loc = optional.name_loc();

                        parameters.push(Parameter::OptionalKeyword(ParameterStruct::new(
                            Offset::from_prism_location(&name_loc),
                            Self::location_to_string(&name_loc).trim_end_matches(':').to_string(),
                        )));
                    }
                    _ => {}
                }
            }

            if let Some(rest) = parameters_list.keyword_rest() {
                match rest {
                    ruby_prism::Node::KeywordRestParameterNode { .. } => {
                        let location = rest
                            .as_keyword_rest_parameter_node()
                            .unwrap()
                            .name_loc()
                            .unwrap_or_else(|| rest.location());

                        parameters.push(Parameter::RestKeyword(ParameterStruct::new(
                            Offset::from_prism_location(&location),
                            Self::location_to_string(&location),
                        )));
                    }
                    ruby_prism::Node::ForwardingParameterNode { .. } => {
                        let location = rest.location();

                        parameters.push(Parameter::Forward(ParameterStruct::new(
                            Offset::from_prism_location(&location),
                            Self::location_to_string(&location),
                        )));
                    }
                    _ => {
                        // Do nothing
                    }
                }
            }

            if let Some(block) = parameters_list.block() {
                let location = block.name_loc().unwrap_or_else(|| block.location());

                parameters.push(Parameter::Block(ParameterStruct::new(
                    Offset::from_prism_location(&location),
                    Self::location_to_string(&location),
                )));
            }
        }

        parameters
    }

    // Runs the given closure if the given call `node` is invoked directly on `self` for each one of its string or
    // symbol arguments
    fn each_string_or_symbol_arg<F>(node: &ruby_prism::CallNode, mut f: F)
    where
        F: FnMut(String, ruby_prism::Location),
    {
        let receiver = node.receiver();

        if (receiver.is_none() || receiver.unwrap().as_self_node().is_some())
            && let Some(arguments) = node.arguments()
        {
            for argument in arguments.arguments().iter() {
                match argument {
                    ruby_prism::Node::SymbolNode { .. } => {
                        let symbol = argument.as_symbol_node().unwrap();

                        if let Some(value_loc) = symbol.value_loc() {
                            let name = Self::location_to_string(&value_loc);
                            f(name, value_loc);
                        }
                    }
                    ruby_prism::Node::StringNode { .. } => {
                        let string = argument.as_string_node().unwrap();
                        let name = String::from_utf8_lossy(string.unescaped()).to_string();
                        f(name, node.location());
                    }
                    _ => {}
                }
            }
        }
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

impl Visit<'_> for RubyIndexer<'_> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'_>) {
        let name = Self::location_to_string(&node.constant_path().location());
        
        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let name_id = NameId::from(&fully_qualified_name);
            let offset = Offset::from_prism_location(&node.location());
            // Uses `location_converter` to evaluate the performance impact of the conversion
            self.location_converter.offset_to_position(&offset).unwrap();

            let definition = Definition::Class(Box::new(ClassDefinition::new(
                name_id,
                indexer.uri_id,
                offset,
            )));

            indexer.local_index.add_definition(fully_qualified_name, definition);

            if let Some(body) = node.body() {
                indexer.visit(&body);
            }
        });
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode) {
        let name = Self::location_to_string(&node.constant_path().location());

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let name_id = NameId::from(&fully_qualified_name);
            let offset = Offset::from_prism_location(&node.location());
            // Uses `location_converter` to evaluate the performance impact of the conversion
            self.location_converter.offset_to_position(&offset).unwrap();
            let definition = Definition::Module(Box::new(ModuleDefinition::new(
                name_id,
                indexer.uri_id,
                offset,
            )));
            indexer.local_index.add_definition(fully_qualified_name, definition);

            if let Some(body) = node.body() {
                indexer.visit(&body);
            }
        });
    }

    fn visit_constant_write_node(&mut self, node: &ruby_prism::ConstantWriteNode) {
        let name_loc = node.name_loc();
        let name = Self::location_to_string(&name_loc);

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let name_id = NameId::from(&fully_qualified_name);
            let offset = Offset::from_prism_location(&name_loc);
            // Uses `location_converter` to evaluate the performance impact of the conversion
            self.location_converter.offset_to_position(&offset).unwrap();
            let definition = Definition::Constant(Box::new(ConstantDefinition::new(
                name_id,
                indexer.uri_id,
                offset,
            )));

            indexer.local_index.add_definition(fully_qualified_name, definition);
        });

        self.visit(&node.value());
    }

    fn visit_constant_path_write_node(&mut self, node: &ruby_prism::ConstantPathWriteNode) {
        let location = node.target().location();
        let name = Self::location_to_string(&location);

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let name_id = NameId::from(&fully_qualified_name);
            let offset = Offset::from_prism_location(&location);
            // Uses `location_converter` to evaluate the performance impact of the conversion
            self.location_converter.offset_to_position(&offset).unwrap();
            let definition = Definition::Constant(Box::new(ConstantDefinition::new(
                name_id,
                indexer.uri_id,
                offset,
            )));

            indexer.local_index.add_definition(fully_qualified_name, definition);
        });

        self.visit(&node.value());
    }

    fn visit_multi_write_node(&mut self, node: &ruby_prism::MultiWriteNode) {
        for left in node.lefts().iter() {
            match left {
                ruby_prism::Node::ConstantTargetNode { .. } | ruby_prism::Node::ConstantPathTargetNode { .. } => {
                    let location = left.location();
                    let name = Self::location_to_string(&location);

                    self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
                        let name_id = NameId::from(&fully_qualified_name);
                        let offset = Offset::from_prism_location(&location);
                        // Uses `location_converter` to evaluate the performance impact of the conversion
                        self.location_converter.offset_to_position(&offset).unwrap();

                        let definition = Definition::Constant(Box::new(ConstantDefinition::new(
                            name_id,
                            indexer.uri_id,
                            offset,
                        )));

                        indexer.local_index.add_definition(fully_qualified_name, definition);
                    });
                }
                ruby_prism::Node::GlobalVariableTargetNode { .. } => {
                    let location = left.location();
                    let name = Self::location_to_string(&location);
                    let name_id = NameId::from(&name);
                    let offset = Offset::from_prism_location(&location);
                    // Uses `location_converter` to evaluate the performance impact of the conversion
                    self.location_converter.offset_to_position(&offset).unwrap();

                    let definition = Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                        name_id,
                        self.uri_id,
                        offset,
                    )));

                    self.local_index.add_definition(name, definition);
                }
                ruby_prism::Node::InstanceVariableTargetNode { .. } => {
                    let location = left.location();
                    let name = Self::location_to_string(&location);

                    self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
                        let name_id = NameId::from(&fully_qualified_name);
                        let offset = Offset::from_prism_location(&location);
                        // Uses `location_converter` to evaluate the performance impact of the conversion
                        self.location_converter.offset_to_position(&offset).unwrap();

                        let definition = Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
                            name_id,
                            indexer.uri_id,
                            offset,
                        )));

                        indexer.local_index.add_definition(fully_qualified_name, definition);
                    });
                }
                ruby_prism::Node::ClassVariableTargetNode { .. } => {
                    let location = left.location();
                    let name = Self::location_to_string(&location);

                    self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
                        let name_id = NameId::from(&fully_qualified_name);
                        let offset = Offset::from_prism_location(&location);
                        // Uses `location_converter` to evaluate the performance impact of the conversion
                        self.location_converter.offset_to_position(&offset).unwrap();

                        let definition = Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
                            name_id,
                            indexer.uri_id,
                            offset,
                        )));

                        indexer.local_index.add_definition(fully_qualified_name, definition);
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
            let name_id = NameId::from(&fully_qualified_name);
            let offset = Offset::from_prism_location(&node.location());
            // Uses `location_converter` to evaluate the performance impact of the conversion
            self.location_converter.offset_to_position(&offset).unwrap();

            let method = MethodDefinition::new(
                name_id,
                indexer.uri_id,
                offset,
                Self::collect_parameters(node),
                node.receiver()
                    .is_some_and(|receiver| receiver.as_self_node().is_some()),
            );

            indexer
                .local_index
                .add_definition(fully_qualified_name, Definition::Method(Box::new(method)));
        });

        if let Some(body) = node.body() {
            self.visit(&body);
        }
    }

    fn visit_call_node(&mut self, node: &ruby_prism::CallNode) {
        let message_loc = node.message_loc();

        if message_loc.is_none() {
            // No message, we can't index this node
            return;
        }

        let message = Self::location_to_string(&message_loc.unwrap());

        match message.as_str() {
            "attr_accessor" => {
                Self::each_string_or_symbol_arg(node, |name, location| {
                    self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
                        let name_id = NameId::from(&fully_qualified_name);
                        let offset = Offset::from_prism_location(&location);
                        // Uses `location_converter` to evaluate the performance impact of the conversion
                        self.location_converter.offset_to_position(&offset).unwrap();

                        indexer.local_index.add_definition(
                            fully_qualified_name.clone(),
                            Definition::AttrAccessor(Box::new(AttrAccessorDefinition::new(
                                name_id,
                                indexer.uri_id,
                                offset,
                            ))),
                        );

                        let writer_name = format!("{fully_qualified_name}=");
                        let writer_name_id = NameId::from(&writer_name);
                        indexer.local_index.add_definition(
                            writer_name,
                            Definition::AttrAccessor(Box::new(AttrAccessorDefinition::new(
                                writer_name_id,
                                indexer.uri_id,
                                Offset::from_prism_location(&location),
                            ))),
                        );
                    });
                });
            }
            "attr_reader" => {
                Self::each_string_or_symbol_arg(node, |name, location| {
                    self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
                        let name_id = NameId::from(&fully_qualified_name);
                        let offset = Offset::from_prism_location(&location);
                        // Uses `location_converter` to evaluate the performance impact of the conversion
                        self.location_converter.offset_to_position(&offset).unwrap();

                        indexer.local_index.add_definition(
                            fully_qualified_name,
                            Definition::AttrReader(Box::new(AttrReaderDefinition::new(
                                name_id,
                                indexer.uri_id,
                                offset,
                            ))),
                        );
                    });
                });
            }
            "attr_writer" => {
                Self::each_string_or_symbol_arg(node, |name, location| {
                    self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
                        let writer_name = format!("{fully_qualified_name}=");
                        let name_id = NameId::from(&writer_name);
                        let offset = Offset::from_prism_location(&location);
                        // Uses `location_converter` to evaluate the performance impact of the conversion
                        self.location_converter.offset_to_position(&offset).unwrap();

                        indexer.local_index.add_definition(
                            writer_name,
                            Definition::AttrWriter(Box::new(AttrWriterDefinition::new(
                                name_id,
                                indexer.uri_id,
                                offset,
                            ))),
                        );
                    });
                });
            }
            _ => {
                // For method calls that we don't explicitly handle each part, we continue visiting their parts as we
                // may discover something inside
                if let Some(receiver) = node.receiver() {
                    self.visit(&receiver);
                }

                if let Some(arguments) = node.arguments() {
                    self.visit_arguments_node(&arguments);
                }

                if let Some(block) = node.block() {
                    self.visit(&block);
                }
            }
        }
    }

    fn visit_global_variable_write_node(&mut self, node: &ruby_prism::GlobalVariableWriteNode) {
        let name_loc = node.name_loc();
        let name = Self::location_to_string(&name_loc);
        let name_id = NameId::from(&name);

        let definition = Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
            name_id,
            self.uri_id,
            Offset::from_prism_location(&name_loc),
        )));

        self.local_index.add_definition(name, definition);

        self.visit(&node.value());
    }

    fn visit_instance_variable_write_node(&mut self, node: &ruby_prism::InstanceVariableWriteNode) {
        let name_loc = node.name_loc();
        let name = Self::location_to_string(&name_loc);

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let name_id = NameId::from(&fully_qualified_name);
            let offset = Offset::from_prism_location(&name_loc);
            // Uses `location_converter` to evaluate the performance impact of the conversion
            self.location_converter.offset_to_position(&offset).unwrap();

            let definition = Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
                name_id,
                indexer.uri_id,
                offset,
            )));

            indexer.local_index.add_definition(fully_qualified_name, definition);
        });

        self.visit(&node.value());
    }

    fn visit_class_variable_write_node(&mut self, node: &ruby_prism::ClassVariableWriteNode) {
        let name_loc = node.name_loc();
        let name = Self::location_to_string(&name_loc);

        self.with_updated_nesting(&name, |indexer, fully_qualified_name| {
            let name_id = NameId::from(&fully_qualified_name);
            let offset = Offset::from_prism_location(&name_loc);
            // Uses `location_converter` to evaluate the performance impact of the conversion
            self.location_converter.offset_to_position(&offset).unwrap();

            let definition = Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
                name_id,
                indexer.uri_id,
                offset,
            )));

            indexer.local_index.add_definition(fully_qualified_name, definition);
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

                match &it.parameters()[0] {
                    Parameter::RequiredPositional(it) => {
                        assert_eq!(it.name(), "a");
                    }
                    _ => panic!("Expected required positional parameter"),
                }

                match &it.parameters()[1] {
                    Parameter::OptionalPositional(it) => {
                        assert_eq!(it.name(), "b");
                    }
                    _ => panic!("Expected optional positional parameter"),
                }

                match &it.parameters()[2] {
                    Parameter::RestPositional(it) => {
                        assert_eq!(it.name(), "c");
                    }
                    _ => panic!("Expected rest positional parameter"),
                }

                match &it.parameters()[3] {
                    Parameter::Post(it) => {
                        assert_eq!(it.name(), "d");
                    }
                    _ => panic!("Expected post parameter"),
                }

                match &it.parameters()[4] {
                    Parameter::RequiredKeyword(it) => {
                        assert_eq!(it.name(), "e");
                    }
                    _ => panic!("Expected required keyword parameter"),
                }

                match &it.parameters()[5] {
                    Parameter::OptionalKeyword(it) => {
                        assert_eq!(it.name(), "g");
                    }
                    _ => panic!("Expected optional keyword parameter"),
                }

                match &it.parameters()[6] {
                    Parameter::RestKeyword(it) => {
                        assert_eq!(it.name(), "i");
                    }
                    _ => panic!("Expected rest keyword parameter"),
                }

                match &it.parameters()[7] {
                    Parameter::Block(it) => {
                        assert_eq!(it.name(), "j");
                    }
                    _ => panic!("Expected block parameter"),
                }
            }
            _ => panic!("Expected method definition"),
        }
    }

    #[test]
    fn index_def_node_with_forward_parameters() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            def foo(...); end
            "
        });

        let definitions = context.graph.get("foo").unwrap();

        match definitions[0] {
            Definition::Method(it) => {
                assert_eq!(it.parameters().len(), 1);
                match &it.parameters()[0] {
                    Parameter::Forward(it) => {
                        assert_eq!(it.name(), "...");
                    }
                    _ => panic!("Expected forward parameter"),
                }
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

            self.attr_accessor :qux
            foo.attr_accessor :not_indexed
            "
        });

        let definitions = context.graph.get("foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 15);
        assert_eq!(definitions[0].end(), 18);

        let definitions = context.graph.get("foo=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 15);
        assert_eq!(definitions[0].end(), 18);

        let definitions = context.graph.get("Foo::bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 47);
        assert_eq!(definitions[0].end(), 50);

        let definitions = context.graph.get("Foo::bar=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 47);
        assert_eq!(definitions[0].end(), 50);

        let definitions = context.graph.get("Foo::baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 53);
        assert_eq!(definitions[0].end(), 56);

        let definitions = context.graph.get("Foo::baz=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 53);
        assert_eq!(definitions[0].end(), 56);

        let definitions = context.graph.get("qux").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 82);
        assert_eq!(definitions[0].end(), 85);

        let definitions = context.graph.get("qux=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 82);
        assert_eq!(definitions[0].end(), 85);

        assert!(context.graph.get("not_indexed").is_none());
        assert!(context.graph.get("not_indexed=").is_none());
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
        assert_eq!(definitions[0].start(), 13);
        assert_eq!(definitions[0].end(), 16);

        assert!(context.graph.get("foo=").is_none());

        let definitions = context.graph.get("Foo::bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 43);
        assert_eq!(definitions[0].end(), 46);

        assert!(context.graph.get("Foo::bar=").is_none());

        let definitions = context.graph.get("Foo::baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 49);
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
        assert_eq!(definitions[0].start(), 13);
        assert_eq!(definitions[0].end(), 16);

        assert!(context.graph.get("foo").is_none());

        let definitions = context.graph.get("Foo::bar=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 43);
        assert_eq!(definitions[0].end(), 46);

        assert!(context.graph.get("Foo::bar").is_none());

        let definitions = context.graph.get("Foo::baz=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start(), 49);
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
