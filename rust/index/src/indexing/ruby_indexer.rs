//! Visit the Ruby AST and create the definitions.

use std::collections::HashMap;

use crate::model::declaration::Declaration;
use crate::model::definitions::{
    AttrAccessorDefinition, AttrReaderDefinition, AttrWriterDefinition, ClassDefinition, ConstantDefinition,
    Definition, MethodDefinition, ModuleDefinition, Parameter, ParameterKind, SingletonClassDefinition,
};
use crate::offset::Offset;
use crate::pools::name_pool::{NameId, NamePool};
use crate::pools::uri_pool::UriId;

use ruby_prism::Visit;

/// The indexer for the definitions found in the Ruby source code.
///
/// It implements the `Visit` trait from `ruby_prism` to visit the AST and create the definitions.
///
/// Example:
///
/// ```
/// use index::indexing::ruby_indexer::RubyIndexer;
/// use ruby_prism::Visit;
/// use std::collections::HashMap;
/// use index::pools::uri_pool::UriPool;
/// use index::pools::name_pool::NamePool;
///
/// let mut name_pool = NamePool::new();
/// let mut uri_pool = UriPool::new();
/// let uri_id = uri_pool.add(String::from("file:///path/to/file1.rb"));
/// let mut definitions = HashMap::new();
/// let mut indexer = RubyIndexer::new(uri_id, &mut name_pool, &mut definitions);
///
/// let source = String::from("class Foo; end");
/// let result = ruby_prism::parse(source.as_ref());
/// indexer.visit(&result.node());
///
/// assert_eq!(definitions.len(), 1);
/// ```
#[derive(Debug)]
pub struct RubyIndexer<'a> {
    uri_id: UriId,
    name_pool: &'a mut NamePool,
    declarations: &'a mut HashMap<NameId, Declaration>,
    name_stack: Vec<String>,
}

impl<'a> RubyIndexer<'a> {
    pub const fn new(
        uri_id: UriId,
        name_pool: &'a mut NamePool,
        declarations: &'a mut HashMap<NameId, Declaration>,
    ) -> Self {
        Self {
            uri_id,
            name_pool,
            declarations,
            name_stack: Vec::new(),
        }
    }

    fn location_to_string(location: &ruby_prism::Location) -> String {
        String::from_utf8_lossy(location.as_slice()).to_string()
    }

    fn location_to_offset(&self, location: &ruby_prism::Location) -> Offset {
        Offset::new(
            self.uri_id,
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

    fn index_definition(&mut self, fully_qualified_name: String, definition: Definition) {
        let name_id = self.name_pool.add(fully_qualified_name);
        let declaration = self.declarations.entry(name_id).or_insert(Declaration::new(name_id));
        declaration.definitions.push(definition);
    }

    fn with_name_pushed<F>(&mut self, name: String, action: F)
    where
        F: FnOnce(&mut Self, String),
    {
        self.name_stack.push(name);
        let fully_qualified_name = self.name_stack.join("::");
        action(self, fully_qualified_name);
        self.name_stack.pop();
    }

    fn with_scoped_name<F>(&mut self, name: String, action: F)
    where
        F: FnOnce(&mut Self, String),
    {
        let mut old_stack = None;

        if name.starts_with("::") {
            old_stack = Some(self.name_stack.clone());
            self.name_stack.clear();
        }

        self.with_name_pushed(name.trim_start_matches("::").to_string(), action);

        if let Some(old_stack) = old_stack {
            self.name_stack = old_stack;
        }
    }

    fn handle_attr_method(&mut self, args: &ruby_prism::ArgumentsNode<'a>, attr_type: &str) {
        for arg in args.arguments().iter() {
            let name = Self::location_to_string(&arg.location());
            self.with_name_pushed(
                name.trim_start_matches(':').to_string(),
                |indexer, fully_qualified_name| {
                    let offset = indexer.location_to_offset(&arg.location());
                    let definition = match attr_type {
                        "attr_reader" => Definition::AttrReader(Box::new(AttrReaderDefinition::new(offset))),
                        "attr_writer" => Definition::AttrWriter(Box::new(AttrWriterDefinition::new(offset))),
                        "attr_accessor" => Definition::AttrAccessor(Box::new(AttrAccessorDefinition::new(offset))),
                        _ => unreachable!("Invalid attr type: {}", attr_type),
                    };
                    indexer.index_definition(fully_qualified_name, definition);
                },
            );
        }
    }

    fn add_parameter(&mut self, parameters: &mut Vec<Parameter>, location: &ruby_prism::Location, kind: ParameterKind) {
        let name = Self::location_to_string(location).trim_end_matches(':').to_string();
        parameters.push(Parameter::new(
            self.name_pool.add(name),
            self.location_to_offset(location),
            kind,
        ));
    }
}

impl<'a> Visit<'a> for RubyIndexer<'a> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'a>) {
        let name = Self::location_to_string(&node.constant_path().location());

        self.with_scoped_name(name, |indexer, fully_qualified_name| {
            let definition = ClassDefinition::new(
                indexer.location_to_offset(&node.location()),
                node.superclass()
                    .map(|name| indexer.name_pool.add(Self::location_to_string(&name.location()))),
            );
            indexer.index_definition(fully_qualified_name, Definition::Class(Box::new(definition)));

            ruby_prism::visit_class_node(indexer, node);
        });
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode<'a>) {
        let name = Self::location_to_string(&node.constant_path().location());

        self.with_scoped_name(name, |indexer, fully_qualified_name| {
            let definition = ModuleDefinition::new(indexer.location_to_offset(&node.location()));
            indexer.index_definition(fully_qualified_name, Definition::Module(Box::new(definition)));

            ruby_prism::visit_module_node(indexer, node);
        });
    }

    fn visit_singleton_class_node(&mut self, node: &ruby_prism::SingletonClassNode<'a>) {
        self.with_name_pushed(String::from("<class>"), |indexer, fully_qualified_name| {
            let definition = SingletonClassDefinition::new(indexer.location_to_offset(&node.location()));
            indexer.index_definition(fully_qualified_name, Definition::SingletonClass(Box::new(definition)));

            ruby_prism::visit_singleton_class_node(indexer, node);
        });
    }

    fn visit_constant_write_node(&mut self, node: &ruby_prism::ConstantWriteNode<'a>) {
        let name = Self::location_to_string(&node.name_loc());

        self.with_name_pushed(name, |indexer, fully_qualified_name| {
            let definition = ConstantDefinition::new(indexer.location_to_offset(&node.location()));
            indexer.index_definition(fully_qualified_name, Definition::Constant(Box::new(definition)));
        });
    }

    fn visit_constant_path_write_node(&mut self, node: &ruby_prism::ConstantPathWriteNode<'a>) {
        let name = Self::location_to_string(&node.target().location());

        self.with_scoped_name(name, |indexer, fully_qualified_name| {
            let definition = ConstantDefinition::new(indexer.location_to_offset(&node.location()));
            indexer.index_definition(fully_qualified_name, Definition::Constant(Box::new(definition)));
        });
    }

    fn visit_multi_write_node(&mut self, node: &ruby_prism::MultiWriteNode<'a>) {
        for left in node.lefts().iter() {
            match left {
                ruby_prism::Node::ConstantTargetNode { .. } | ruby_prism::Node::ConstantPathTargetNode { .. } => {
                    let name = Self::location_to_string(&left.location());

                    self.with_scoped_name(name, |indexer, fully_qualified_name| {
                        let definition = ConstantDefinition::new(indexer.location_to_offset(&left.location()));
                        indexer.index_definition(fully_qualified_name, Definition::Constant(Box::new(definition)));
                    });
                }
                _ => {}
            }
        }
    }

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode<'a>) {
        let mut parameters: Vec<Parameter> = Vec::new();
        if let Some(parameters_list) = node.parameters() {
            for parameter in parameters_list.requireds().iter() {
                self.add_parameter(
                    &mut parameters,
                    &parameter.location(),
                    ParameterKind::RequiredPositional,
                );
            }
            for parameter in parameters_list.optionals().iter() {
                if let ruby_prism::Node::OptionalParameterNode { .. } = parameter {
                    let opt_param = parameter.as_optional_parameter_node().unwrap();
                    self.add_parameter(
                        &mut parameters,
                        &opt_param.name_loc(),
                        ParameterKind::OptionalPositional,
                    );
                }
            }
            if let Some(rest) = parameters_list.rest() {
                let rest_param = rest.as_rest_parameter_node().unwrap();
                let name_location = rest_param.name_loc().unwrap_or_else(|| rest.location());
                self.add_parameter(&mut parameters, &name_location, ParameterKind::RestPositional);
            }
            for post in parameters_list.posts().iter() {
                self.add_parameter(&mut parameters, &post.location(), ParameterKind::Post);
            }
            for keyword in parameters_list.keywords().iter() {
                match keyword {
                    ruby_prism::Node::RequiredKeywordParameterNode { .. } => {
                        let required = keyword.as_required_keyword_parameter_node().unwrap();
                        self.add_parameter(&mut parameters, &required.name_loc(), ParameterKind::RequiredKeyword);
                    }
                    ruby_prism::Node::OptionalKeywordParameterNode { .. } => {
                        let optional = keyword.as_optional_keyword_parameter_node().unwrap();
                        self.add_parameter(&mut parameters, &optional.name_loc(), ParameterKind::OptionalKeyword);
                    }
                    _ => {}
                }
            }
            if let Some(rest) = parameters_list.keyword_rest() {
                let rest_param = rest.as_keyword_rest_parameter_node().unwrap();
                let name_location = rest_param.name_loc().unwrap_or_else(|| rest.location());
                self.add_parameter(&mut parameters, &name_location, ParameterKind::RestKeyword);
            }
            if let Some(block) = parameters_list.block() {
                let name_location = block.name_loc().unwrap_or_else(|| block.location());
                self.add_parameter(&mut parameters, &name_location, ParameterKind::Block);
            }
        }

        self.with_name_pushed(
            Self::location_to_string(&node.name_loc()),
            |indexer, fully_qualified_name| {
                let method = MethodDefinition::new(
                    indexer.location_to_offset(&node.location()),
                    parameters,
                    node.receiver()
                        .is_some_and(|receiver| receiver.as_self_node().is_some()),
                );

                indexer.index_definition(fully_qualified_name, Definition::Method(Box::new(method)));
            },
        );
    }

    fn visit_call_node(&mut self, node: &ruby_prism::CallNode<'a>) {
        if node
            .receiver()
            .is_some_and(|receiver| receiver.as_self_node().is_none())
        {
            return;
        }

        if let Some(args) = node.arguments() {
            match String::from_utf8_lossy(node.name().as_slice()).as_ref() {
                "attr_reader" | "attr_writer" | "attr_accessor" => {
                    let attr_type = String::from_utf8_lossy(node.name().as_slice());
                    self.handle_attr_method(&args, &attr_type);
                }
                _ => {}
            }
        }

        ruby_prism::visit_call_node(self, node);
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
        declarations: HashMap<String, Declaration>,
    }

    impl Context {
        fn new() -> Self {
            Self {
                uri_pool: UriPool::new(),
                name_pool: NamePool::new(),
                declarations: HashMap::new(),
            }
        }

        fn declaration(&self, name: &str) -> Option<&Declaration> {
            self.declarations.get(name)
        }

        fn assert_single_definition(&self, name: &str) -> &Declaration {
            let declaration = self
                .declaration(name)
                .unwrap_or_else(|| panic!("Expected declaration for '{name}'"));
            assert_eq!(declaration.definitions.len(), 1);
            declaration
        }

        fn assert_class_definition(&self, name: &str) {
            let declaration = self.assert_single_definition(name);
            assert!(
                declaration.definitions[0].as_class().is_some(),
                "Expected '{name}' to be a class definition"
            );
        }
        fn assert_module_definition(&self, name: &str) {
            let declaration = self.assert_single_definition(name);
            assert!(
                declaration.definitions[0].as_module().is_some(),
                "Expected '{name}' to be a module definition"
            );
        }

        fn assert_singleton_class_definition(&self, name: &str) {
            let declaration = self.assert_single_definition(name);
            assert!(
                declaration.definitions[0].as_singleton_class().is_some(),
                "Expected '{name}' to be a singleton class definition"
            );
        }

        fn assert_method_definition(&self, name: &str) {
            let declaration = self.assert_single_definition(name);
            assert!(
                declaration.definitions[0].as_method().is_some(),
                "Expected '{name}' to be a method definition"
            );
        }

        fn assert_attr_reader_definition(&self, name: &str) {
            let declaration = self.assert_single_definition(name);
            assert!(
                declaration.definitions[0].as_attr_reader().is_some(),
                "Expected '{name}' to be an attr_reader definition"
            );
        }

        fn assert_attr_writer_definition(&self, name: &str) {
            let declaration = self.assert_single_definition(name);
            assert!(
                declaration.definitions[0].as_attr_writer().is_some(),
                "Expected '{name}' to be an attr_writer definition"
            );
        }

        fn assert_attr_accessor_definition(&self, name: &str) {
            let declaration = self.assert_single_definition(name);
            assert!(
                declaration.definitions[0].as_attr_accessor().is_some(),
                "Expected '{name}' to be an attr_accessor definition"
            );
        }
    }

    fn parse_string(string: &str) -> Context {
        let mut declarations = HashMap::new();
        let mut context = Context::new();

        let uri_id = context.uri_pool.add("file:///test.rb".to_string());
        let mut ruby_indexer = RubyIndexer::new(uri_id, &mut context.name_pool, &mut declarations);
        let result = ruby_prism::parse(string.as_bytes());
        ruby_indexer.visit(&result.node());

        for (name_id, declaration) in declarations {
            context
                .declarations
                .insert(name_id.to_string(&context.name_pool), declaration);
        }

        context
    }

    #[test]
    fn test_index_ruby_class() {
        let context = parse_string("class Foo; end");

        context.assert_class_definition("Foo");
        // Verify no superclass
        let declaration = context.assert_single_definition("Foo");
        match &declaration.definitions[0] {
            Definition::Class(definition) => {
                assert_eq!(definition.superclass, None);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_index_ruby_class_with_constant_path() {
        let context = parse_string("class Foo::Bar; end");

        context.assert_class_definition("Foo::Bar");
        // Verify no superclass
        let declaration = context.assert_single_definition("Foo::Bar");
        match &declaration.definitions[0] {
            Definition::Class(definition) => {
                assert_eq!(definition.superclass, None);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_index_ruby_class_with_superclass() {
        let context = parse_string("class Foo < ::Bar; end");

        let declaration = context.declaration("Foo").unwrap();
        assert_eq!(declaration.definitions.len(), 1);
        match &declaration.definitions[0] {
            Definition::Class(definition) => {
                assert_eq!(definition.superclass.unwrap().to_string(&context.name_pool), "::Bar");
            }
            _ => panic!("Expected a class definition"),
        }
    }

    #[test]
    fn test_index_ruby_module() {
        let context = parse_string("module Foo; end");

        context.assert_module_definition("Foo");
    }

    #[test]
    fn test_index_ruby_module_with_constant_path() {
        let context = parse_string("module Foo::Bar; end");

        context.assert_module_definition("Foo::Bar");
    }

    #[test]
    fn test_index_ruby_singleton_class() {
        let context = parse_string("class << self; end");

        assert_eq!(context.declarations.len(), 1);

        let declaration = context.declaration("<class>").unwrap();
        assert_eq!(declaration.definitions.len(), 1);
        assert!(declaration.definitions[0].as_singleton_class().is_some());
    }

    #[test]
    fn test_index_ruby_nested_scopes() {
        let context = parse_string(
            "
                module Foo
                  class Bar
                    class << self; end
                  end
                end
            ",
        );

        assert_eq!(context.declarations.len(), 3);
        context.assert_module_definition("Foo");
        context.assert_class_definition("Foo::Bar");
        context.assert_singleton_class_definition("Foo::Bar::<class>");
    }

    #[test]
    fn test_index_ruby_nested_scopes_with_correct_names() {
        let context = parse_string(
            "
                module Foo
                  class ::Bar
                    class Baz; end
                  end

                  class Qux; end
                end
            ",
        );

        assert_eq!(context.declarations.len(), 4);

        assert!(context.declaration("Foo").is_some());
        assert!(context.declaration("Bar").is_some());
        assert!(context.declaration("Bar::Baz").is_some());
        assert!(context.declaration("Foo::Qux").is_some());
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

        assert_eq!(context.declarations.len(), 4);

        assert!(
            context.declaration("FOO").unwrap().definitions[0]
                .as_constant()
                .is_some()
        );

        assert!(
            context.declaration("BAR::BAZ").unwrap().definitions[0]
                .as_constant()
                .is_some()
        );

        assert!(
            context.declaration("CONST1").unwrap().definitions[0]
                .as_constant()
                .is_some()
        );

        assert!(
            context.declaration("CONST2").unwrap().definitions[0]
                .as_constant()
                .is_some()
        );
    }

    #[test]
    fn test_builder_method() {
        let context = parse_string("def foo; end");

        assert_eq!(context.declarations.len(), 1);

        match &context.declaration("foo").unwrap().definitions[0] {
            Definition::Method(definition) => {
                assert_eq!(definition.parameters.len(), 0);
                assert!(!definition.is_singleton);
            }
            _ => panic!("Expected a method definition"),
        }
    }

    #[test]
    fn test_builder_method_with_receiver() {
        let context = parse_string(
            "
                def self.foo; end
                def Foo.bar; end
                def foo.baz; end
            ",
        );

        assert_eq!(context.declarations.len(), 3);

        match &context.declaration("foo").unwrap().definitions[0] {
            Definition::Method(definition) => {
                assert!(definition.is_singleton);
            }
            _ => panic!("Expected a method definition"),
        }

        match &context.declaration("bar").unwrap().definitions[0] {
            Definition::Method(definition) => {
                assert!(!definition.is_singleton);
            }
            _ => panic!("Expected a method definition"),
        }

        match &context.declaration("baz").unwrap().definitions[0] {
            Definition::Method(definition) => {
                assert!(!definition.is_singleton);
            }
            _ => panic!("Expected a method definition"),
        }
    }

    #[test]
    fn test_builder_method_with_all_named_parameters() {
        let context = parse_string("def foo(p1, p2 = 42, *p3, p4, p5:, p6: 42, **p7, &block); end");

        assert_eq!(context.declarations.len(), 1);

        match &context.declaration("foo").unwrap().definitions[0] {
            Definition::Method(definition) => {
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

        assert_eq!(context.declarations.len(), 1);

        match &context.declaration("foo").unwrap().definitions[0] {
            Definition::Method(definition) => {
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

    #[test]
    fn test_index_ruby_attr_accessors() {
        let context = parse_string(
            "
                attr_reader :foo
                attr_writer :bar
                attr_accessor :baz
            ",
        );

        assert_eq!(context.declarations.len(), 3);
        context.assert_attr_reader_definition("foo");
        context.assert_attr_writer_definition("bar");
        context.assert_attr_accessor_definition("baz");
    }

    #[test]
    fn test_index_ruby_attr_accessors_with_multiple_names() {
        let context = parse_string(
            "
                attr_reader :a1, :a2
                attr_writer :b1, :b2
                attr_accessor :c1, :c2
            ",
        );

        assert_eq!(context.declarations.len(), 6);

        assert!(
            context.declaration("a1").unwrap().definitions[0]
                .as_attr_reader()
                .is_some()
        );
        assert!(
            context.declaration("a2").unwrap().definitions[0]
                .as_attr_reader()
                .is_some()
        );

        assert!(
            context.declaration("b1").unwrap().definitions[0]
                .as_attr_writer()
                .is_some()
        );

        assert!(
            context.declaration("c1").unwrap().definitions[0]
                .as_attr_accessor()
                .is_some()
        );

        assert!(
            context.declaration("c2").unwrap().definitions[0]
                .as_attr_accessor()
                .is_some()
        );
    }
}
