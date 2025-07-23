//! Parse the source code and build the symbol definitions.

use crate::model::symbol_definitions::{
    ClassDefinition, ConstantDefinition, MethodDefinition, ModuleDefinition, Parameter, ParameterKind,
    SingletonClassDefinition, SymbolDefinition,
};
use crate::offset::Offset;
use crate::pools::name_pool::{NameId, NamePool};
use crate::pools::uri_pool::UriId;
use ruby_prism::Visit;

/// The builder for the symbol definitions.
///
/// It implements the `Visit` trait from `ruby_prism` to visit the AST and build the symbol definitions.
#[derive(Debug)]
pub struct Builder<'a> {
    uri_id: UriId,
    name_pool: &'a mut NamePool,
    symbol_definitions: &'a mut Vec<SymbolDefinition>,
}

impl<'a> Builder<'a> {
    /// Creates a new builder.
    ///
    /// # Arguments
    ///
    /// * `uri_id` - The ID of the uri to build the symbol definitions for
    /// * `name_pool` - The name pool to use for the symbol definitions
    /// * `symbol_definitions` - The vector to store the symbol definitions created by this builder
    ///
    /// # Returns
    ///
    /// A new builder.
    ///
    /// Example:
    ///
    /// ```
    /// let mut name_pool = NamePool::new();
    /// let mut symbol_definitions = Vec::new();
    /// let builder = Builder::new(uri_id, &mut name_pool, &mut symbol_definitions);
    ///
    /// let result = ruby_prism::parse("class Foo; end");
    /// builder.visit(&result.node());
    ///
    /// assert_eq!(symbol_definitions.len(), 1);
    /// ```
    pub const fn new(
        uri_id: UriId,
        name_pool: &'a mut NamePool,
        symbol_definitions: &'a mut Vec<SymbolDefinition>,
    ) -> Self {
        Self {
            uri_id,
            name_pool,
            symbol_definitions,
        }
    }

    /// Interns a location slice into a name ID.
    ///
    /// # Arguments
    ///
    /// * `location` - The Prism location of the string to intern
    ///
    /// # Returns
    ///
    /// The name ID of the interned string.
    fn intern_slice(&mut self, location: &ruby_prism::Location) -> NameId {
        self.name_pool
            .add(String::from_utf8_lossy(location.as_slice()).to_string())
    }

    fn intern_slice_without_last_char(&mut self, location: &ruby_prism::Location) -> NameId {
        let mut s = String::from_utf8_lossy(location.as_slice()).to_string();
        s.pop(); // Remove the last character
        self.name_pool.add(s)
    }

    /// Converts a Prism location to an offset.
    ///
    /// # Arguments
    ///
    /// * `location` - The Prism location to convert
    ///
    /// # Returns
    ///
    /// The offset of the location.
    fn prism_location_to_offset(&self, location: &ruby_prism::Location) -> Offset {
        Offset::new(
            self.uri_id,
            location.start_offset().try_into().unwrap(),
            location.end_offset().try_into().unwrap(),
        )
    }
}

impl<'a> Visit<'a> for Builder<'a> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'a>) {
        let class = ClassDefinition::new(
            self.intern_slice(&node.constant_path().location()),
            self.prism_location_to_offset(&node.location()),
            node.superclass().map(|name| self.intern_slice(&name.location())),
        );

        self.symbol_definitions.push(SymbolDefinition::Class(Box::new(class)));

        ruby_prism::visit_class_node(self, node);
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode<'a>) {
        let module = ModuleDefinition::new(
            self.intern_slice(&node.constant_path().location()),
            self.prism_location_to_offset(&node.location()),
        );

        self.symbol_definitions.push(SymbolDefinition::Module(Box::new(module)));

        ruby_prism::visit_module_node(self, node);
    }

    fn visit_singleton_class_node(&mut self, node: &ruby_prism::SingletonClassNode<'a>) {
        let singleton = SingletonClassDefinition::new(self.prism_location_to_offset(&node.location()));

        self.symbol_definitions
            .push(SymbolDefinition::SingletonClass(Box::new(singleton)));

        ruby_prism::visit_singleton_class_node(self, node);
    }

    fn visit_constant_write_node(&mut self, node: &ruby_prism::ConstantWriteNode<'a>) {
        let constant = ConstantDefinition::new(
            self.intern_slice(&node.name_loc()),
            self.prism_location_to_offset(&node.location()),
        );

        self.symbol_definitions
            .push(SymbolDefinition::Constant(Box::new(constant)));
    }

    fn visit_constant_path_write_node(&mut self, node: &ruby_prism::ConstantPathWriteNode<'a>) {
        let constant = ConstantDefinition::new(
            self.intern_slice(&node.target().location()),
            self.prism_location_to_offset(&node.location()),
        );

        self.symbol_definitions
            .push(SymbolDefinition::Constant(Box::new(constant)));
    }

    fn visit_multi_write_node(&mut self, node: &ruby_prism::MultiWriteNode<'a>) {
        for left in node.lefts().iter() {
            match left {
                ruby_prism::Node::ConstantTargetNode { .. } | ruby_prism::Node::ConstantPathTargetNode { .. } => {
                    let constant = ConstantDefinition::new(
                        self.intern_slice(&left.location()),
                        self.prism_location_to_offset(&left.location()),
                    );

                    self.symbol_definitions
                        .push(SymbolDefinition::Constant(Box::new(constant)));
                }
                _ => {}
            }
        }
    }

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode<'a>) {
        let mut parameters: Vec<Parameter> = Vec::new();
        if let Some(parameters_list) = node.parameters() {
            for parameter in parameters_list.requireds().iter() {
                parameters.push(Parameter::new(
                    self.intern_slice(&parameter.location()),
                    self.prism_location_to_offset(&parameter.location()),
                    ParameterKind::RequiredPositional,
                ));
            }
            for parameter in parameters_list.optionals().iter() {
                if let ruby_prism::Node::OptionalParameterNode { .. } = parameter {
                    let opt_param = parameter.as_optional_parameter_node().unwrap();
                    parameters.push(Parameter::new(
                        self.intern_slice(&opt_param.name_loc()),
                        self.prism_location_to_offset(&parameter.location()),
                        ParameterKind::OptionalPositional,
                    ));
                }
            }
            if let Some(rest) = parameters_list.rest() {
                let rest_param = rest.as_rest_parameter_node().unwrap();
                parameters.push(Parameter::new(
                    self.intern_slice(&rest_param.name_loc().unwrap_or_else(|| rest.location())),
                    self.prism_location_to_offset(&rest.location()),
                    ParameterKind::RestPositional,
                ));
            }
            for post in parameters_list.posts().iter() {
                parameters.push(Parameter::new(
                    self.intern_slice(&post.location()),
                    self.prism_location_to_offset(&post.location()),
                    ParameterKind::Post,
                ));
            }
            for keyword in parameters_list.keywords().iter() {
                match keyword {
                    ruby_prism::Node::RequiredKeywordParameterNode { .. } => {
                        let required = keyword.as_required_keyword_parameter_node().unwrap();
                        parameters.push(Parameter::new(
                            self.intern_slice_without_last_char(&required.name_loc()),
                            self.prism_location_to_offset(&keyword.location()),
                            ParameterKind::RequiredKeyword,
                        ));
                    }
                    ruby_prism::Node::OptionalKeywordParameterNode { .. } => {
                        let optional = keyword.as_optional_keyword_parameter_node().unwrap();
                        parameters.push(Parameter::new(
                            self.intern_slice_without_last_char(&optional.name_loc()),
                            self.prism_location_to_offset(&keyword.location()),
                            ParameterKind::OptionalKeyword,
                        ));
                    }
                    _ => {}
                }
            }
            if let Some(rest) = parameters_list.keyword_rest() {
                let rest_param = rest.as_keyword_rest_parameter_node().unwrap();
                parameters.push(Parameter::new(
                    self.intern_slice(&rest_param.name_loc().unwrap_or_else(|| rest.location())),
                    self.prism_location_to_offset(&rest.location()),
                    ParameterKind::RestKeyword,
                ));
            }
            if let Some(block) = parameters_list.block() {
                parameters.push(Parameter::new(
                    self.intern_slice(&block.name_loc().unwrap_or_else(|| block.location())),
                    self.prism_location_to_offset(&block.location()),
                    ParameterKind::Block,
                ));
            }
        }

        let method = MethodDefinition::new(
            self.intern_slice(&node.name_loc()),
            self.prism_location_to_offset(&node.location()),
            parameters,
        );

        self.symbol_definitions.push(SymbolDefinition::Method(Box::new(method)));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pools::string_pool::PoolId;
    use crate::pools::uri_pool::UriPool;

    struct Context {
        uri_pool: UriPool,
        name_pool: NamePool,
        symbol_definitions: Vec<SymbolDefinition>,
    }

    impl Context {
        fn new() -> Self {
            Self {
                uri_pool: UriPool::new(),
                name_pool: NamePool::new(),
                symbol_definitions: Vec::new(),
            }
        }
    }

    fn parse_string(string: &str) -> Context {
        let mut context = Context::new();

        let uri_id = context.uri_pool.add("file:///test.rb".to_string());
        let mut builder = Builder::new(uri_id, &mut context.name_pool, &mut context.symbol_definitions);
        let result = ruby_prism::parse(string.as_bytes());
        builder.visit(&result.node());

        context
    }

    #[test]
    fn test_builder_class() {
        let context = parse_string("class Foo; end");

        assert_eq!(context.symbol_definitions.len(), 1);

        match &context.symbol_definitions[0] {
            SymbolDefinition::Class(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "Foo");
                assert_eq!(definition.superclass, None);
            }
            _ => panic!("Expected a class definition"),
        }
    }

    #[test]
    fn test_builder_class_with_constant_path() {
        let context = parse_string("class Foo::Bar; end");

        assert_eq!(context.symbol_definitions.len(), 1);

        match &context.symbol_definitions[0] {
            SymbolDefinition::Class(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "Foo::Bar");
                assert_eq!(definition.superclass, None);
            }
            _ => panic!("Expected a class definition"),
        }
    }

    #[test]
    fn test_builder_class_with_superclass() {
        let context = parse_string("class Foo < ::Bar; end");

        assert_eq!(context.symbol_definitions.len(), 1);

        match &context.symbol_definitions[0] {
            SymbolDefinition::Class(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "Foo");
                assert_eq!(definition.superclass.unwrap().to_string(&context.name_pool), "::Bar");
            }
            _ => panic!("Expected a class definition"),
        }
    }

    #[test]
    fn test_builder_module() {
        let context = parse_string("module Foo; end");

        assert_eq!(context.symbol_definitions.len(), 1);

        match &context.symbol_definitions[0] {
            SymbolDefinition::Module(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "Foo");
            }
            _ => panic!("Expected a module definition"),
        }
    }

    #[test]
    fn test_builder_module_with_constant_path() {
        let context = parse_string("module Foo::Bar; end");

        assert_eq!(context.symbol_definitions.len(), 1);

        #[allow(clippy::match_wildcard_for_single_variants)]
        match &context.symbol_definitions[0] {
            SymbolDefinition::Module(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "Foo::Bar");
            }
            _ => panic!("Expected a module definition"),
        }
    }

    #[test]
    fn test_builder_singleton_class() {
        let context = parse_string("class << self; end");

        assert_eq!(context.symbol_definitions.len(), 1);

        match &context.symbol_definitions[0] {
            SymbolDefinition::SingletonClass(_) => {}
            _ => panic!("Expected a singleton class definition"),
        }
    }

    #[test]
    fn test_builder_nested_scopes() {
        let context = parse_string(
            "
                module Foo
                  class Bar
                    class << self; end
                  end
                end
            ",
        );

        assert_eq!(context.symbol_definitions.len(), 3);

        match &context.symbol_definitions[0] {
            SymbolDefinition::Module(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "Foo");
            }
            _ => panic!("Expected a module definition"),
        }

        match &context.symbol_definitions[1] {
            SymbolDefinition::Class(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "Bar");
            }
            _ => panic!("Expected a class definition"),
        }

        match &context.symbol_definitions[2] {
            SymbolDefinition::SingletonClass(_) => {}
            _ => panic!("Expected a singleton class definition"),
        }
    }

    #[test]
    fn test_builder_constant() {
        let context = parse_string(
            "
                FOO = 1
                ::BAR::BAZ = 2
                CONST1, ::CONST2 = 3
            ",
        );

        assert_eq!(context.symbol_definitions.len(), 4);

        match &context.symbol_definitions[0] {
            SymbolDefinition::Constant(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "FOO");
            }
            _ => panic!("Expected a constant definition"),
        }

        match &context.symbol_definitions[1] {
            SymbolDefinition::Constant(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "::BAR::BAZ");
            }
            _ => panic!("Expected a constant definition"),
        }

        match &context.symbol_definitions[2] {
            SymbolDefinition::Constant(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "CONST1");
            }
            _ => panic!("Expected a constant definition"),
        }

        match &context.symbol_definitions[3] {
            SymbolDefinition::Constant(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "::CONST2");
            }
            _ => panic!("Expected a constant definition"),
        }
    }

    #[test]
    fn test_builder_method() {
        let context = parse_string("def foo; end");

        assert_eq!(context.symbol_definitions.len(), 1);

        match &context.symbol_definitions[0] {
            SymbolDefinition::Method(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "foo");
                assert_eq!(definition.parameters.len(), 0);
            }
            _ => panic!("Expected a method definition"),
        }
    }

    #[test]
    fn test_builder_method_with_all_named_parameters() {
        let context = parse_string("def foo(p1, p2 = 42, *p3, p4, p5:, p6: 42, **p7, &block); end");

        assert_eq!(context.symbol_definitions.len(), 1);

        match &context.symbol_definitions[0] {
            SymbolDefinition::Method(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "foo");
                assert_eq!(definition.parameters.len(), 8);

                assert_eq!(definition.parameters[0].name_id.to_string(&context.name_pool), "p1");
                assert_eq!(definition.parameters[0].kind, ParameterKind::RequiredPositional);
                assert_eq!(definition.parameters[1].name_id.to_string(&context.name_pool), "p2");
                assert_eq!(definition.parameters[1].kind, ParameterKind::OptionalPositional);
                assert_eq!(definition.parameters[2].name_id.to_string(&context.name_pool), "p3");
                assert_eq!(definition.parameters[2].kind, ParameterKind::RestPositional);
                assert_eq!(definition.parameters[3].name_id.to_string(&context.name_pool), "p4");
                assert_eq!(definition.parameters[3].kind, ParameterKind::Post);
                assert_eq!(definition.parameters[4].name_id.to_string(&context.name_pool), "p5");
                assert_eq!(definition.parameters[4].kind, ParameterKind::RequiredKeyword);
                assert_eq!(definition.parameters[5].name_id.to_string(&context.name_pool), "p6");
                assert_eq!(definition.parameters[5].kind, ParameterKind::OptionalKeyword);
                assert_eq!(definition.parameters[6].name_id.to_string(&context.name_pool), "p7");
                assert_eq!(definition.parameters[6].kind, ParameterKind::RestKeyword);
                assert_eq!(definition.parameters[7].name_id.to_string(&context.name_pool), "block");
                assert_eq!(definition.parameters[7].kind, ParameterKind::Block);
            }
            _ => panic!("Expected a method definition"),
        }
    }

    #[test]
    fn test_builder_method_with_anonymous_parameters() {
        let context = parse_string("def foo(*, **, &); end");

        assert_eq!(context.symbol_definitions.len(), 1);

        match &context.symbol_definitions[0] {
            SymbolDefinition::Method(definition) => {
                assert_eq!(definition.name_id.to_string(&context.name_pool), "foo");
                assert_eq!(definition.parameters.len(), 3);

                assert_eq!(definition.parameters[0].name_id.to_string(&context.name_pool), "*");
                assert_eq!(definition.parameters[0].kind, ParameterKind::RestPositional);
                assert_eq!(definition.parameters[1].name_id.to_string(&context.name_pool), "**");
                assert_eq!(definition.parameters[1].kind, ParameterKind::RestKeyword);
                assert_eq!(definition.parameters[2].name_id.to_string(&context.name_pool), "&");
                assert_eq!(definition.parameters[2].kind, ParameterKind::Block);
            }
            _ => panic!("Expected a method definition"),
        }
    }

    // TODO: def self.foo; end
}
