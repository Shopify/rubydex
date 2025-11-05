//! Visit the Ruby AST and create the definitions.

use std::sync::{Arc, LazyLock};

use crate::errors::Errors;
use crate::indexing::scope::Scope;
use crate::model::comment::Comment;
use crate::model::definitions::{
    AttrAccessorDefinition, AttrReaderDefinition, AttrWriterDefinition, ClassDefinition, ClassVariableDefinition,
    ConstantDefinition, Definition, GlobalVariableDefinition, InstanceVariableDefinition, MethodDefinition,
    ModuleDefinition, Parameter, ParameterStruct,
};
use crate::model::graph::Graph;
use crate::model::ids::{DeclarationId, UriId};
use crate::model::references::{ConstantReference, MethodReference, UnresolvedReference};
use crate::offset::Offset;

use ruby_prism::{ParseResult, Visit};

pub type IndexerParts = (Graph, Vec<Errors>);
static OBJECT_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Object"));
static MAIN_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("<main>"));

/// The indexer for the definitions found in the Ruby source code.
///
/// It implements the `Visit` trait from `ruby_prism` to visit the AST and create a hash of definitions that must be
/// merged into the global state later.
pub struct RubyIndexer<'a> {
    uri_id: UriId,
    local_graph: Graph,
    errors: Vec<Errors>,
    comments: Vec<CommentGroup>,
    source: &'a str,
    scope: Scope,
}

impl<'a> RubyIndexer<'a> {
    #[must_use]
    pub fn new(uri: String, source: &'a str) -> Self {
        let mut local_index = Graph::new();
        let uri_id = local_index.add_uri(uri);

        Self {
            uri_id,
            local_graph: local_index,
            errors: Vec::new(),
            comments: Vec::new(),
            source,
            scope: Scope::new(),
        }
    }

    #[must_use]
    pub fn into_parts(self) -> IndexerParts {
        (self.local_graph, self.errors)
    }

    pub fn index(&mut self) {
        let result = ruby_prism::parse(self.source.as_bytes());
        self.comments = self.parse_comments_into_groups(&result);
        self.visit(&result.node());
    }

    fn parse_comments_into_groups(&mut self, result: &ParseResult<'_>) -> Vec<CommentGroup> {
        let mut iter = result.comments().peekable();
        let mut groups = Vec::new();

        while let Some(comment) = iter.next() {
            let mut group = CommentGroup::new();
            group.add_comment(&comment);
            while let Some(next_comment) = iter.peek() {
                if group.accepts(next_comment, self.source) {
                    let next = iter.next().unwrap();
                    group.add_comment(&next);
                } else {
                    break;
                }
            }
            groups.push(group);
        }
        groups
    }

    fn location_to_string(location: &ruby_prism::Location) -> String {
        String::from_utf8_lossy(location.as_slice()).to_string()
    }

    fn find_comments_for(&self, offset: u32) -> Option<Vec<Comment>> {
        let offset_usize = offset as usize;
        if self.comments.is_empty() {
            return None;
        }

        let idx = match self.comments.binary_search_by_key(&offset_usize, |g| g.end_offset) {
            Ok(_) => {
                // This should never happen in valid Ruby syntax - a comment cannot end exactly
                // where a definition begins (there must be at least a newline between them)
                debug_assert!(false, "Comment ends exactly at definition start - this indicates a bug");
                return None;
            }
            Err(i) if i > 0 => i - 1,
            Err(_) => return None,
        };

        let group = &self.comments[idx];
        let between = &self.source.as_bytes()[group.end_offset..offset_usize];
        if !between.iter().all(|&b| b.is_ascii_whitespace()) {
            return None;
        }

        // We allow at most one blank line between the comment and the definition
        if bytecount::count(between, b'\n') > 2 {
            return None;
        }

        if group.comments.is_empty() {
            None
        } else {
            Some(group.comments.clone())
        }
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
                        f(name, argument.location());
                    }
                    _ => {}
                }
            }
        }
    }

    fn index_constant_reference(&mut self, location: &ruby_prism::Location) {
        let offset = Offset::from_prism_location(location);
        let name = Self::location_to_string(location);

        let (name, scope) = if let Some(trimmed) = name.strip_prefix("::") {
            (trimmed.to_string(), None)
        } else {
            (name, self.scope.nesting().as_ref().map(Arc::clone))
        };

        let name_id = self.local_graph.add_name(name);
        let reference =
            UnresolvedReference::Constant(Box::new(ConstantReference::new(name_id, scope, self.uri_id, offset)));
        self.local_graph.add_unresolved_reference(reference);
    }

    fn index_method_reference(&mut self, name: String, location: &ruby_prism::Location) {
        let offset = Offset::from_prism_location(location);
        let name_id = self.local_graph.add_name(name);
        let reference = UnresolvedReference::Method(Box::new(MethodReference::new(name_id, self.uri_id, offset)));
        self.local_graph.add_unresolved_reference(reference);
    }
}

struct CommentGroup {
    end_offset: usize,
    comments: Vec<Comment>,
}

impl CommentGroup {
    #[must_use]
    pub fn new() -> Self {
        Self {
            end_offset: 0,
            comments: Vec::new(),
        }
    }

    // Accepts the next line if it is continuous
    fn accepts(&self, next: &ruby_prism::Comment, source: &str) -> bool {
        let current_end_offset = self.end_offset;
        let next_line_start_offset = next.location().start_offset();

        let between = &source.as_bytes()[current_end_offset..next_line_start_offset];
        if !between.iter().all(|&b| b.is_ascii_whitespace()) {
            return false;
        }

        // If there is at most one newline between the two texts,
        // that means two texts are continuous
        bytecount::count(between, b'\n') <= 1
    }

    // For the magic comments, what we want to do is the following:
    // 1. still move the group end offset to the end of the magic comment
    // 2. not add the comment to the comments array
    fn add_comment(&mut self, comment: &ruby_prism::Comment) {
        self.end_offset = comment.location().end_offset();
        let text = String::from_utf8_lossy(comment.location().as_slice()).to_string();
        self.comments.push(Comment::new(
            Offset::from_prism_location(&comment.location()),
            text.trim().to_string(),
        ));
    }
}

impl Visit<'_> for RubyIndexer<'_> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'_>) {
        let previous_nesting_id = self.scope.current_nesting_id().unwrap_or(*OBJECT_ID);
        let name = Self::location_to_string(&node.constant_path().location());
        let (fully_qualified_name, declaration_id) = self.scope.enter(&name);
        let offset = Offset::from_prism_location(&node.location());
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();

        let definition = Definition::Class(Box::new(ClassDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));

        self.local_graph
            .add_definition(fully_qualified_name, definition, &previous_nesting_id);
        self.local_graph.add_member(&previous_nesting_id, declaration_id, &name);

        if let ruby_prism::Node::ConstantPathNode { .. } = node.constant_path() {
            let constant_path = node.constant_path().as_constant_path_node().unwrap();
            if let Some(parent) = constant_path.parent() {
                self.visit(&parent);
            }
        }

        if let Some(superclass) = node.superclass() {
            self.visit(&superclass);
        }

        if let Some(body) = node.body() {
            self.visit(&body);
        }

        self.scope.leave();
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode) {
        let previous_nesting_id = self.scope.current_nesting_id().unwrap_or(*OBJECT_ID);
        let name = Self::location_to_string(&node.constant_path().location());
        let (fully_qualified_name, declaration_id) = self.scope.enter(&name);
        let offset = Offset::from_prism_location(&node.location());
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();

        let definition = Definition::Module(Box::new(ModuleDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));
        self.local_graph
            .add_definition(fully_qualified_name, definition, &previous_nesting_id);
        self.local_graph.add_member(&previous_nesting_id, declaration_id, &name);

        if let ruby_prism::Node::ConstantPathNode { .. } = node.constant_path() {
            let constant_path = node.constant_path().as_constant_path_node().unwrap();
            if let Some(parent) = constant_path.parent() {
                self.visit(&parent);
            }
        }

        if let Some(body) = node.body() {
            self.visit(&body);
        }

        self.scope.leave();
    }

    fn visit_constant_and_write_node(&mut self, node: &ruby_prism::ConstantAndWriteNode) {
        self.index_constant_reference(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_constant_operator_write_node(&mut self, node: &ruby_prism::ConstantOperatorWriteNode) {
        self.index_constant_reference(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_constant_or_write_node(&mut self, node: &ruby_prism::ConstantOrWriteNode) {
        self.index_constant_reference(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_constant_write_node(&mut self, node: &ruby_prism::ConstantWriteNode) {
        let previous_nesting_id = self.scope.current_nesting_id().unwrap_or(*OBJECT_ID);
        let name_loc = node.name_loc();
        let name = Self::location_to_string(&name_loc);
        let fully_qualified_name = self.scope.fully_qualify(&name);

        let declaration_id = DeclarationId::from(&fully_qualified_name);
        let offset = Offset::from_prism_location(&name_loc);
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();

        let definition = Definition::Constant(Box::new(ConstantDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));

        self.local_graph
            .add_definition(fully_qualified_name, definition, &previous_nesting_id);
        self.local_graph.add_member(&previous_nesting_id, declaration_id, &name);
        self.visit(&node.value());
    }

    fn visit_constant_path_and_write_node(&mut self, node: &ruby_prism::ConstantPathAndWriteNode) {
        self.visit_constant_path_node(&node.target());
    }

    fn visit_constant_path_operator_write_node(&mut self, node: &ruby_prism::ConstantPathOperatorWriteNode) {
        self.visit_constant_path_node(&node.target());
    }

    fn visit_constant_path_or_write_node(&mut self, node: &ruby_prism::ConstantPathOrWriteNode) {
        self.visit_constant_path_node(&node.target());
    }

    fn visit_constant_path_write_node(&mut self, node: &ruby_prism::ConstantPathWriteNode) {
        let previous_nesting_id = self.scope.current_nesting_id().unwrap_or(*OBJECT_ID);
        let location = node.target().location();
        let name = Self::location_to_string(&location);
        let fully_qualified_name = self.scope.fully_qualify(&name);

        let declaration_id = DeclarationId::from(&fully_qualified_name);
        let offset = Offset::from_prism_location(&location);
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();

        let definition = Definition::Constant(Box::new(ConstantDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));

        self.local_graph
            .add_definition(fully_qualified_name, definition, &previous_nesting_id);
        self.local_graph.add_member(&previous_nesting_id, declaration_id, &name);

        if let Some(parent) = node.target().parent() {
            self.visit(&parent);
        }
        self.visit(&node.value());
    }

    fn visit_constant_read_node(&mut self, node: &ruby_prism::ConstantReadNode<'_>) {
        self.index_constant_reference(&node.location());
    }

    fn visit_constant_path_node(&mut self, node: &ruby_prism::ConstantPathNode<'_>) {
        if let Some(parent) = node.parent() {
            self.visit(&parent);
        }

        self.index_constant_reference(&node.location());
    }

    fn visit_multi_write_node(&mut self, node: &ruby_prism::MultiWriteNode) {
        for left in node.lefts().iter() {
            match left {
                ruby_prism::Node::ConstantTargetNode { .. } | ruby_prism::Node::ConstantPathTargetNode { .. } => {
                    let previous_nesting_id = self.scope.current_nesting_id().unwrap_or(*OBJECT_ID);
                    let location = left.location();
                    let name = Self::location_to_string(&location);
                    let fully_qualified_name = self.scope.fully_qualify(&name);

                    let declaration_id = DeclarationId::from(&fully_qualified_name);
                    let offset = Offset::from_prism_location(&location);
                    let comments = self.find_comments_for(offset.start()).unwrap_or_default();

                    let definition = Definition::Constant(Box::new(ConstantDefinition::new(
                        declaration_id,
                        self.uri_id,
                        offset,
                        comments,
                    )));

                    self.local_graph
                        .add_definition(fully_qualified_name, definition, &previous_nesting_id);
                    self.local_graph.add_member(&previous_nesting_id, declaration_id, &name);
                }
                ruby_prism::Node::GlobalVariableTargetNode { .. } => {
                    let location = left.location();
                    let name = Self::location_to_string(&location);
                    let declaration_id = DeclarationId::from(&name);
                    let offset = Offset::from_prism_location(&location);

                    let comments = self.find_comments_for(offset.start()).unwrap_or_default();
                    let definition = Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                        declaration_id,
                        self.uri_id,
                        offset,
                        comments,
                    )));

                    self.local_graph.add_member(&MAIN_ID, declaration_id, &name);
                    self.local_graph.add_definition(name, definition, &MAIN_ID);
                }
                ruby_prism::Node::InstanceVariableTargetNode { .. } => {
                    let nesting_id = self.scope.current_nesting_id().unwrap_or(*MAIN_ID);
                    let location = left.location();
                    let name = Self::location_to_string(&location);
                    let fully_qualified_name = self.scope.fully_qualify(&name);

                    let declaration_id = DeclarationId::from(&fully_qualified_name);
                    let offset = Offset::from_prism_location(&location);
                    let comments = self.find_comments_for(offset.start()).unwrap_or_default();

                    let definition = Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
                        declaration_id,
                        self.uri_id,
                        offset,
                        comments,
                    )));

                    self.local_graph
                        .add_definition(fully_qualified_name, definition, &nesting_id);
                    self.local_graph.add_member(&nesting_id, declaration_id, &name);
                }
                ruby_prism::Node::ClassVariableTargetNode { .. } => {
                    if let Some(nesting_id) = self.scope.current_nesting_id() {
                        let location = left.location();
                        let name = Self::location_to_string(&location);
                        let fully_qualified_name = self.scope.fully_qualify(&name);

                        let declaration_id = DeclarationId::from(&fully_qualified_name);
                        let offset = Offset::from_prism_location(&location);
                        let comments = self.find_comments_for(offset.start()).unwrap_or_default();

                        let definition = Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
                            declaration_id,
                            self.uri_id,
                            offset,
                            comments,
                        )));

                        self.local_graph.add_member(&nesting_id, declaration_id, &name);
                        self.local_graph
                            .add_definition(fully_qualified_name, definition, &nesting_id);
                    }
                }
                ruby_prism::Node::CallTargetNode { .. } => {
                    let call_target_node = left.as_call_target_node().unwrap();
                    self.visit(&call_target_node.receiver());

                    let name = String::from_utf8_lossy(call_target_node.name().as_slice()).to_string();
                    self.index_method_reference(name, &call_target_node.location());
                }
                _ => {}
            }
        }

        self.visit(&node.value());
    }

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode) {
        let nesting_id = self.scope.current_nesting_id().unwrap_or(*MAIN_ID);
        let name = Self::location_to_string(&node.name_loc());
        let fully_qualified_name = self.scope.fully_qualify(&name);

        let declaration_id = DeclarationId::from(&fully_qualified_name);
        let offset = Offset::from_prism_location(&node.location());
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();

        let method = MethodDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            Self::collect_parameters(node),
            node.receiver()
                .is_some_and(|receiver| receiver.as_self_node().is_some()),
            comments,
        );

        self.local_graph
            .add_definition(fully_qualified_name, Definition::Method(Box::new(method)), &nesting_id);
        self.local_graph.add_member(&nesting_id, declaration_id, &name);

        if let Some(body) = node.body() {
            self.visit(&body);
        }
    }

    #[allow(clippy::too_many_lines)]
    fn visit_call_node(&mut self, node: &ruby_prism::CallNode) {
        fn create_attr_accessor(indexer: &mut RubyIndexer, node: &ruby_prism::CallNode) {
            RubyIndexer::each_string_or_symbol_arg(node, |name, location| {
                if let Some(nesting_id) = indexer.scope.current_nesting_id() {
                    let fully_qualified_name = indexer.scope.fully_qualify(&name);
                    let declaration_id = DeclarationId::from(&fully_qualified_name);
                    let writer_name = format!("{fully_qualified_name}=");
                    let writer_declaration_id = DeclarationId::from(&writer_name);
                    let offset = Offset::from_prism_location(&location);
                    let comments = indexer.find_comments_for(offset.start()).unwrap_or_default();

                    indexer.local_graph.add_member(&nesting_id, declaration_id, &name);
                    indexer.local_graph.add_definition(
                        fully_qualified_name,
                        Definition::AttrAccessor(Box::new(AttrAccessorDefinition::new(
                            declaration_id,
                            indexer.uri_id,
                            offset,
                            comments.clone(),
                        ))),
                        &nesting_id,
                    );

                    indexer
                        .local_graph
                        .add_member(&nesting_id, writer_declaration_id, &format!("{name}="));
                    indexer.local_graph.add_definition(
                        writer_name,
                        Definition::AttrAccessor(Box::new(AttrAccessorDefinition::new(
                            writer_declaration_id,
                            indexer.uri_id,
                            Offset::from_prism_location(&location),
                            comments,
                        ))),
                        &nesting_id,
                    );
                }
            });
        }

        fn create_attr_reader(indexer: &mut RubyIndexer, node: &ruby_prism::CallNode) {
            RubyIndexer::each_string_or_symbol_arg(node, |name, location| {
                if let Some(nesting_id) = indexer.scope.current_nesting_id() {
                    let fully_qualified_name = indexer.scope.fully_qualify(&name);
                    let declaration_id = DeclarationId::from(&fully_qualified_name);
                    let offset = Offset::from_prism_location(&location);
                    let comments = indexer.find_comments_for(offset.start()).unwrap_or_default();

                    indexer.local_graph.add_definition(
                        fully_qualified_name,
                        Definition::AttrReader(Box::new(AttrReaderDefinition::new(
                            declaration_id,
                            indexer.uri_id,
                            offset,
                            comments,
                        ))),
                        &nesting_id,
                    );
                    indexer.local_graph.add_member(&nesting_id, declaration_id, &name);
                }
            });
        }

        fn create_attr_writer(indexer: &mut RubyIndexer, node: &ruby_prism::CallNode) {
            RubyIndexer::each_string_or_symbol_arg(node, |name, location| {
                if let Some(nesting_id) = indexer.scope.current_nesting_id() {
                    let fully_qualified_name = indexer.scope.fully_qualify(&name);
                    let writer_name = format!("{fully_qualified_name}=");
                    let declaration_id = DeclarationId::from(&writer_name);
                    let offset = Offset::from_prism_location(&location);
                    let comments = indexer.find_comments_for(offset.start()).unwrap_or_default();

                    indexer.local_graph.add_definition(
                        writer_name,
                        Definition::AttrWriter(Box::new(AttrWriterDefinition::new(
                            declaration_id,
                            indexer.uri_id,
                            offset,
                            comments,
                        ))),
                        &nesting_id,
                    );
                    indexer
                        .local_graph
                        .add_member(&nesting_id, declaration_id, &format!("{name}="));
                }
            });
        }

        let message_loc = node.message_loc();

        if message_loc.is_none() {
            // No message, we can't index this node
            return;
        }

        let message = String::from_utf8_lossy(node.name().as_slice()).to_string();

        match message.as_str() {
            "attr_accessor" => {
                create_attr_accessor(self, node);
            }
            "attr_reader" => {
                create_attr_reader(self, node);
            }
            "attr_writer" => {
                create_attr_writer(self, node);
            }
            "attr" => {
                // attr :foo, true        => both reader and writer
                // attr :foo, false       => only reader
                // attr :foo              => only reader
                // attr :foo, "bar", :baz => only readers for foo, bar, and baz
                let create_writer = if let Some(arguments) = node.arguments() {
                    let args_vec: Vec<_> = arguments.arguments().iter().collect();
                    matches!(args_vec.as_slice(), [_, ruby_prism::Node::TrueNode { .. }])
                } else {
                    false
                };

                if create_writer {
                    create_attr_accessor(self, node);
                } else {
                    create_attr_reader(self, node);
                }
            }
            "alias_method" => {
                let arguments = node.arguments();

                if let Some(arguments) = arguments {
                    let nodes: Vec<_> = arguments.arguments().iter().collect();

                    if nodes.len() == 2
                        && let Some(symbol_node) = nodes[1].as_symbol_node()
                    {
                        let value_loc = symbol_node.value_loc().unwrap();
                        let alias_to = Self::location_to_string(&value_loc);
                        self.index_method_reference(alias_to, &value_loc);
                    }
                }
            }
            _ => {
                // For method calls that we don't explicitly handle each part, we continue visiting their parts as we
                // may discover something inside
                if let Some(receiver) = node.receiver() {
                    self.visit(&receiver);
                }

                self.index_method_reference(message.clone(), &node.message_loc().unwrap());

                match message.as_str() {
                    ">" | "<" | ">=" | "<=" => {
                        self.index_method_reference("<=>".to_string(), &node.message_loc().unwrap());
                    }
                    _ => {}
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

    fn visit_call_and_write_node(&mut self, node: &ruby_prism::CallAndWriteNode) {
        if let Some(receiver) = node.receiver() {
            self.visit(&receiver);
        }

        let read_name = String::from_utf8_lossy(node.read_name().as_slice()).to_string();
        self.index_method_reference(read_name, &node.operator_loc());

        let write_name = String::from_utf8_lossy(node.write_name().as_slice()).to_string();
        self.index_method_reference(write_name, &node.operator_loc());

        self.visit(&node.value());
    }

    fn visit_call_operator_write_node(&mut self, node: &ruby_prism::CallOperatorWriteNode) {
        if let Some(receiver) = node.receiver() {
            self.visit(&receiver);
        }

        let read_name = String::from_utf8_lossy(node.read_name().as_slice()).to_string();
        self.index_method_reference(read_name, &node.call_operator_loc().unwrap());

        let write_name = String::from_utf8_lossy(node.write_name().as_slice()).to_string();
        self.index_method_reference(write_name, &node.call_operator_loc().unwrap());

        self.visit(&node.value());
    }

    fn visit_call_or_write_node(&mut self, node: &ruby_prism::CallOrWriteNode) {
        if let Some(receiver) = node.receiver() {
            self.visit(&receiver);
        }

        let read_name = String::from_utf8_lossy(node.read_name().as_slice()).to_string();
        self.index_method_reference(read_name, &node.operator_loc());

        let write_name = String::from_utf8_lossy(node.write_name().as_slice()).to_string();
        self.index_method_reference(write_name, &node.operator_loc());

        self.visit(&node.value());
    }

    fn visit_global_variable_write_node(&mut self, node: &ruby_prism::GlobalVariableWriteNode) {
        let name_loc = node.name_loc();
        let name = Self::location_to_string(&name_loc);
        let declaration_id = DeclarationId::from(&name);
        let offset = Offset::from_prism_location(&name_loc);

        let comments = self.find_comments_for(offset.start()).unwrap_or_default();
        let definition = Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));

        self.local_graph.add_member(&MAIN_ID, declaration_id, &name);
        self.local_graph.add_definition(name, definition, &MAIN_ID);
        self.visit(&node.value());
    }

    fn visit_instance_variable_write_node(&mut self, node: &ruby_prism::InstanceVariableWriteNode) {
        let nesting_id = self.scope.current_nesting_id().unwrap_or(*MAIN_ID);
        let name_loc = node.name_loc();
        let name = Self::location_to_string(&name_loc);
        let fully_qualified_name = self.scope.fully_qualify(&name);

        let declaration_id = DeclarationId::from(&fully_qualified_name);
        let offset = Offset::from_prism_location(&name_loc);
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();

        let definition = Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));

        self.local_graph
            .add_definition(fully_qualified_name, definition, &nesting_id);
        self.local_graph.add_member(&nesting_id, declaration_id, &name);
        self.visit(&node.value());
    }

    fn visit_class_variable_write_node(&mut self, node: &ruby_prism::ClassVariableWriteNode) {
        // Ruby immediately crashes when defining class variables on <main>
        if let Some(nesting_id) = self.scope.current_nesting_id() {
            let name_loc = node.name_loc();
            let name = Self::location_to_string(&name_loc);
            let fully_qualified_name = self.scope.fully_qualify(&name);

            let declaration_id = DeclarationId::from(&fully_qualified_name);
            let offset = Offset::from_prism_location(&name_loc);
            let comments = self.find_comments_for(offset.start()).unwrap_or_default();

            let definition = Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
                declaration_id,
                self.uri_id,
                offset,
                comments,
            )));

            self.local_graph.add_member(&nesting_id, declaration_id, &name);
            self.local_graph
                .add_definition(fully_qualified_name, definition, &nesting_id);
            self.visit(&node.value());
        }
    }

    fn visit_block_argument_node(&mut self, node: &ruby_prism::BlockArgumentNode<'_>) {
        let expression = node.expression();
        if let Some(expression) = expression {
            match expression {
                ruby_prism::Node::SymbolNode { .. } => {
                    let symbol = expression.as_symbol_node().unwrap();
                    let name = Self::location_to_string(&symbol.value_loc().unwrap());
                    self.index_method_reference(name, &node.location());
                }
                _ => {
                    self.visit(&expression);
                }
            }
        }
    }

    fn visit_alias_method_node(&mut self, node: &ruby_prism::AliasMethodNode<'_>) {
        let name = Self::location_to_string(&node.old_name().location());
        self.index_method_reference(name, &node.old_name().location());
    }

    fn visit_and_node(&mut self, node: &ruby_prism::AndNode) {
        self.visit(&node.left());
        self.index_method_reference("&&".to_string(), &node.location());
        self.visit(&node.right());
    }

    fn visit_or_node(&mut self, node: &ruby_prism::OrNode) {
        self.visit(&node.left());
        self.index_method_reference("||".to_string(), &node.location());
        self.visit(&node.right());
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;
    use crate::{model::ids::NameId, test_utils::GraphTest};

    /// Asserts that a definition exists, matches the expected type, and passes a custom assertion
    macro_rules! assert_definition {
        ($context:expr, $variant:ident, $name:expr, $assertion:expr) => {
            let defs = $context.graph.get($name).unwrap();
            match &defs[0] {
                Definition::$variant(_) => {
                    $assertion(&defs[0]);
                }
                _ => panic!("Expected {} definition for '{}'", stringify!($variant), $name),
            }
        };
    }

    /// Asserts that a definition has specific comments
    macro_rules! assert_definition_comments {
        ($context:expr, $variant:ident, $name:expr, $expected_comments:expr) => {
            let expected_vec: Vec<&str> = $expected_comments;

            assert_definition!($context, $variant, $name, |def: &Definition| {
                assert_eq!(
                    def.comments()
                        .iter()
                        .map(Comment::string)
                        .collect::<Vec<&String>>(),
                    expected_vec
                );
            });
        };
    }

    /// Asserts that a definition has specific start and end offsets
    macro_rules! assert_definition_offset {
        ($definition:expr, $start:expr, $end:expr) => {
            assert_eq!($definition.start(), $start);
            assert_eq!($definition.end(), $end);
        };
    }

    /// Asserts that the given fully qualified name is owned by the second fully qualified name
    macro_rules! assert_owner {
        ($graph:expr, $declaration_name:expr, $owner_name:expr) => {
            let declaration = $graph
                .declarations()
                .get(&DeclarationId::from($declaration_name))
                .unwrap();
            assert_eq!(declaration.owner_id(), &DeclarationId::from($owner_name));
        };
    }

    fn collect_unresolved_references(graph: &Graph) -> Vec<&UnresolvedReference> {
        let mut unresolved_references = graph.unresolved_references().values().collect::<Vec<_>>();

        unresolved_references.sort_by_key(|r| match r {
            UnresolvedReference::Constant(constant) => (
                graph.documents().get(&constant.uri_id()).unwrap().uri().to_string(),
                constant.offset().start(),
                graph.names().get(constant.name_id()).unwrap(),
            ),
            UnresolvedReference::Method(method) => (
                graph.documents().get(&method.uri_id()).unwrap().uri().to_string(),
                method.offset().start(),
                graph.names().get(method.name_id()).unwrap(),
            ),
        });

        unresolved_references
    }

    fn collect_constant_reference_names(graph: &Graph) -> Vec<&String> {
        collect_unresolved_references(graph)
            .iter()
            .filter_map(|r| match r {
                UnresolvedReference::Constant(constant) => Some(graph.names().get(constant.name_id()).unwrap()),
                UnresolvedReference::Method(_method) => None,
            })
            .collect::<Vec<_>>()
    }

    fn collect_method_reference_names(graph: &Graph) -> Vec<&String> {
        collect_unresolved_references(graph)
            .iter()
            .filter_map(|r| match r {
                UnresolvedReference::Constant(_) => None,
                UnresolvedReference::Method(method) => Some(graph.names().get(method.name_id()).unwrap()),
            })
            .collect::<Vec<_>>()
    }

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
        assert_definition_offset!(definitions[0], 0, 50);
        assert_owner!(context.graph, "Foo", "Object");

        let definitions = context.graph.get("Foo::Bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 12, 46);
        assert_owner!(context.graph, "Foo::Bar", "Foo");

        let definitions = context.graph.get("Foo::Bar::Baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 26, 40);
        assert_owner!(context.graph, "Foo::Bar::Baz", "Foo::Bar");

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
        assert_definition_offset!(definitions[0], 0, 53);
        assert_owner!(context.graph, "Foo", "Object");

        let definitions = context.graph.get("Foo::Bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 13, 49);
        assert_owner!(context.graph, "Foo::Bar", "Foo");

        let definitions = context.graph.get("Foo::Bar::Baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 28, 43);
        assert_owner!(context.graph, "Foo::Bar::Baz", "Foo::Bar");

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
        assert_definition_offset!(definitions[0], 0, 3);
        assert_owner!(context.graph, "FOO", "Object");

        let definitions = context.graph.get("Foo::FOO").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 21, 24);
        assert_owner!(context.graph, "Foo::FOO", "Foo");
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
        assert_definition_offset!(definitions[0], 0, 8);

        let definitions = context.graph.get("Foo::FOO::BAR").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 26, 34);

        let definitions = context.graph.get("BAZ").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 41, 46);
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
        assert_definition_offset!(definitions[0], 0, 3);

        let definitions = context.graph.get("BAR::BAZ").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 5, 13);

        let definitions = context.graph.get("Foo::FOO").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 34, 37);

        let definitions = context.graph.get("Foo::BAR::BAZ").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 39, 47);

        let definitions = context.graph.get("BAZ").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 49, 54);
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
        assert_definition_offset!(definitions[0], 0, 12);
        assert_owner!(context.graph, "foo", "<main>");

        match definitions[0] {
            Definition::Method(it) => {
                assert_eq!(it.parameters().len(), 0);
                assert!(!it.is_singleton());
            }
            _ => panic!("Expected method definition"),
        }

        let definitions = context.graph.get("Foo::bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 26, 38);
        assert_owner!(context.graph, "Foo::bar", "Foo");

        match definitions[0] {
            Definition::Method(it) => {
                assert_eq!(it.parameters().len(), 0);
                assert!(!it.is_singleton());
            }
            _ => panic!("Expected method definition"),
        }

        let definitions = context.graph.get("Foo::baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 41, 58);

        // FIXME: The owner is wrong until we implement singletons
        assert_owner!(context.graph, "Foo::baz", "Foo");

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
            class Foo
              attr_accessor :bar, :baz
            end

            foo.attr_accessor :not_indexed
            "
        });

        let definitions = context.graph.get("Foo::bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 27, 30);
        assert_owner!(context.graph, "Foo::bar", "Foo");

        let definitions = context.graph.get("Foo::bar=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 27, 30);
        assert_owner!(context.graph, "Foo::bar=", "Foo");

        let definitions = context.graph.get("Foo::baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 33, 36);
        assert_owner!(context.graph, "Foo::baz", "Foo");

        let definitions = context.graph.get("Foo::baz=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 33, 36);
        assert_owner!(context.graph, "Foo::baz=", "Foo");

        assert!(context.graph.get("not_indexed").is_none());
        assert!(context.graph.get("not_indexed=").is_none());
    }

    #[test]
    fn index_comments_attached_to_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            # Single comment
            class Single; end

            # Multi-line comment 1
            # Multi-line comment 2
            # Multi-line comment 3
            module Multi; end

            # Comment 1
            #
            # Comment 2
            class EmptyCommentLine; end

            # Comment directly above (no gap)
            NoGap = 42

            #: ()
            #| -> void
            def foo; end

            # Comment with blank line

            class BlankLine; end

            # Too far away


            class NoComment; end
            "
        });

        assert_definition_comments!(context, Class, "Single", vec!["# Single comment"]);
        assert_definition_comments!(
            context,
            Module,
            "Multi",
            vec![
                "# Multi-line comment 1",
                "# Multi-line comment 2",
                "# Multi-line comment 3"
            ]
        );
        assert_definition_comments!(
            context,
            Class,
            "EmptyCommentLine",
            vec!["# Comment 1", "#", "# Comment 2"]
        );
        assert_definition_comments!(context, Constant, "NoGap", vec!["# Comment directly above (no gap)"]);
        assert_definition_comments!(context, Method, "foo", vec!["#: ()", "#| -> void"]);
        assert_definition_comments!(context, Class, "BlankLine", vec!["# Comment with blank line"]);
        assert_definition_comments!(context, Class, "NoComment", vec![]);
    }

    #[test]
    fn index_comments_indented_and_nested() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            # Outer class
            class Outer
              # Inner class at 2 spaces
              class Inner
                # Deep class at 4 spaces
                class Deep; end
              end

              # Another inner class
              # with multiple lines
              class AnotherInner; end
            end
            "
        });

        assert_definition_comments!(context, Class, "Outer", vec!["# Outer class"]);
        assert_definition_comments!(context, Class, "Outer::Inner", vec!["# Inner class at 2 spaces"]);
        assert_definition_comments!(context, Class, "Outer::Inner::Deep", vec!["# Deep class at 4 spaces"]);
        assert_definition_comments!(
            context,
            Class,
            "Outer::AnotherInner",
            vec!["# Another inner class", "# with multiple lines"]
        );
    }

    #[test]
    fn index_attr_reader_definition() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            class Foo
              attr_reader :bar, :baz
            end
            "
        });

        let definitions = context.graph.get("Foo::bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 25, 28);
        assert_owner!(context.graph, "Foo::bar", "Foo");

        assert!(context.graph.get("Foo::bar=").is_none());

        let definitions = context.graph.get("Foo::baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 31, 34);
        assert_owner!(context.graph, "Foo::baz", "Foo");

        assert!(context.graph.get("Foo::baz=").is_none());
    }

    #[test]
    fn index_attr_writer_definition() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            class Foo
              attr_writer :bar, :baz
            end
            "
        });

        let definitions = context.graph.get("Foo::bar=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 25, 28);
        assert_owner!(context.graph, "Foo::bar=", "Foo");

        assert!(context.graph.get("Foo::bar").is_none());

        let definitions = context.graph.get("Foo::baz=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 31, 34);
        assert_owner!(context.graph, "Foo::baz=", "Foo");

        assert!(context.graph.get("Foo::baz").is_none());
    }

    #[test]
    fn ignores_attributes_defined_on_main() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            "
            attr_writer :foo
            attr_reader :bar
            attr_accessor :baz
            attr :qux, true
            "
        });
        assert!(context.graph.get("foo=").is_none());
        assert!(context.graph.get("bar").is_none());
        assert!(context.graph.get("baz").is_none());
        assert!(context.graph.get("baz=").is_none());
        assert!(context.graph.get("qux").is_none());
        assert!(context.graph.get("qux=").is_none());
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
        assert_definition_offset!(definitions[0], 0, 4);
        assert_owner!(context.graph, "$foo", "<main>");

        let definitions = context.graph.get("$bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 9, 13);
        assert_owner!(context.graph, "$bar", "<main>");

        let definitions = context.graph.get("$baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 15, 19);
        assert_owner!(context.graph, "$baz", "<main>");

        let definitions = context.graph.get("$qux").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 40, 44);
        assert_owner!(context.graph, "$qux", "<main>");
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
        assert_definition_offset!(definitions[0], 0, 4);
        assert_owner!(context.graph, "@foo", "<main>");

        let definitions = context.graph.get("Foo::@bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 22, 26);
        assert_owner!(context.graph, "Foo::@bar", "Foo");

        let definitions = context.graph.get("Foo::@baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 34, 38);
        assert_owner!(context.graph, "Foo::@baz", "Foo");

        let definitions = context.graph.get("Foo::@qux").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 40, 44);
        assert_owner!(context.graph, "Foo::@qux", "Foo");
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

        assert!(context.graph.get("@@foo").is_none());

        let definitions = context.graph.get("Foo::@@bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 23, 28);
        assert_owner!(context.graph, "Foo::@@bar", "Foo");

        let definitions = context.graph.get("Foo::@@baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 36, 41);
        assert_owner!(context.graph, "Foo::@@baz", "Foo");

        let definitions = context.graph.get("Foo::@@qux").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 43, 48);
        assert_owner!(context.graph, "Foo::@@qux", "Foo");
    }

    #[test]
    fn index_attr_with_no_parameter() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            r#"
            class Foo
              attr "foo"
              attr :bar
            end
            "#
        });

        for reader_name in ["Foo::foo", "Foo::bar"] {
            let definitions = context.graph.get(reader_name).unwrap();
            assert_eq!(definitions.len(), 1);
            assert!(matches!(definitions[0], Definition::AttrReader(_)));
            assert_owner!(context.graph, reader_name, "Foo");
        }

        for writer_name in ["Foo::foo=", "Foo::bar="] {
            assert!(context.graph.get(writer_name).is_none());
        }
    }

    #[test]
    fn index_attr_with_false_parameter() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            r#"
            class Foo
              attr "foo", false
              attr :bar, false
            end
            "#
        });

        for reader_name in ["Foo::foo", "Foo::bar"] {
            let definitions = context.graph.get(reader_name).unwrap();
            assert_eq!(definitions.len(), 1);
            assert!(matches!(definitions[0], Definition::AttrReader(_)));
        }

        for writer_name in ["Foo::foo=", "Foo::bar="] {
            assert!(context.graph.get(writer_name).is_none());
        }
    }

    #[test]
    fn index_attr_with_true_parameter() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            r#"
            class Foo
              attr "foo", true
              attr :bar, true
            end
            "#
        });

        for name in ["Foo::foo", "Foo::foo=", "Foo::bar", "Foo::bar="] {
            let definitions = context.graph.get(name).unwrap();
            assert_eq!(definitions.len(), 1);
            assert!(matches!(definitions[0], Definition::AttrAccessor(_)));
        }
    }

    #[test]
    fn index_attr_with_string_and_symbol_parameter() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            r#"
            class Foo
              attr "foo", :bar, :baz
              attr :a, "b", "c", :d
            end
            "#
        });

        for name in [
            "Foo::foo", "Foo::bar", "Foo::baz", "Foo::a", "Foo::b", "Foo::c", "Foo::d",
        ] {
            let definitions = context.graph.get(name).unwrap();
            assert_eq!(definitions.len(), 1);
            assert!(matches!(definitions[0], Definition::AttrReader(_)));
        }

        for writer_name in [
            "Foo::foo=",
            "Foo::bar=",
            "Foo::baz=",
            "Foo::a=",
            "Foo::b=",
            "Foo::c=",
            "Foo::d=",
        ] {
            assert!(context.graph.get(writer_name).is_none());
        }
    }

    #[test]
    fn rejects_attr_with_incorrect_parameter() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            r#"
            class Foo
              attr "foo", 123
            end
            "#
        });

        let definitions = context.graph.get("Foo::foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert!(matches!(definitions[0], Definition::AttrReader(_)));

        assert!(context.graph.get("Foo::foo=").is_none());
        assert!(context.graph.get("Foo::123").is_none());
    }

    #[test]
    fn constant_members() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
              class Baz; end

              A = 1
              B::C = 2
              D, E::F = 3, 4
            end
            "
        });

        let foo = context.graph.declarations().get(&DeclarationId::from("Foo")).unwrap();
        let members = foo.members();

        // FIX ME: missing assert that `Foo` is owned by `Object`. Depends on the RBS crate!
        // let object = context.graph.declarations().get(&DeclarationId::from("Object")).unwrap();
        // let object_members = object.members();
        // assert!(object_members.contains_key(&NameId::from("Foo")));

        assert_eq!(members.len(), 6);
        assert_eq!(
            members.get(&NameId::from("Bar")).unwrap(),
            &DeclarationId::from("Foo::Bar")
        );
        assert_eq!(
            members.get(&NameId::from("Baz")).unwrap(),
            &DeclarationId::from("Foo::Baz")
        );
        assert_eq!(members.get(&NameId::from("A")).unwrap(), &DeclarationId::from("Foo::A"));
        assert_eq!(members.get(&NameId::from("D")).unwrap(), &DeclarationId::from("Foo::D"));
        assert_eq!(
            members.get(&NameId::from("B::C")).unwrap(),
            &DeclarationId::from("Foo::B::C")
        );
        assert_eq!(
            members.get(&NameId::from("E::F")).unwrap(),
            &DeclarationId::from("Foo::E::F")
        );
    }

    #[test]
    fn constant_path_members() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
              class Bar::Baz; end
            end
            "
        });

        // FIXME: We are currently not handling constant paths correctly. `Bar::Baz` defines `Baz` under `Bar`, but we
        // right now define `Bar::Baz` under `Foo`
        let bar = context
            .graph
            .declarations()
            .get(&DeclarationId::from("Foo::Bar"))
            .unwrap();
        let members = bar.members();

        assert_eq!(members.len(), 0);

        let foo = context.graph.declarations().get(&DeclarationId::from("Foo")).unwrap();
        let members = foo.members();

        assert_eq!(members.len(), 2);
        assert_eq!(
            members.get(&NameId::from("Bar::Baz")).unwrap(),
            &DeclarationId::from("Foo::Bar::Baz")
        );
    }

    #[test]
    fn instance_variable_members() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              @bar = 1
              @baz, @qux = 2, 3
            end
            "
        });

        let foo = context.graph.declarations().get(&DeclarationId::from("Foo")).unwrap();
        let members = foo.members();
        assert_eq!(members.len(), 3);
        assert_eq!(
            members.get(&NameId::from("@bar")).unwrap(),
            &DeclarationId::from("Foo::@bar")
        );
        assert_eq!(
            members.get(&NameId::from("@baz")).unwrap(),
            &DeclarationId::from("Foo::@baz")
        );
        assert_eq!(
            members.get(&NameId::from("@qux")).unwrap(),
            &DeclarationId::from("Foo::@qux")
        );
    }

    #[test]
    fn method_members() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def bar; end
            end
            "
        });

        let foo = context.graph.declarations().get(&DeclarationId::from("Foo")).unwrap();
        let members = foo.members();
        assert_eq!(members.len(), 1);
        assert_eq!(
            members.get(&NameId::from("bar")).unwrap(),
            &DeclarationId::from("Foo::bar")
        );
    }

    #[test]
    fn global_variable_members() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            $var = 1
            class Foo
              $var2, $var3 = 2
            end
            "
        });

        let declarations = context.graph.declarations();
        let main = declarations.get(&DeclarationId::from("<main>")).unwrap();
        let members = main.members();
        assert_eq!(members.len(), 3);
        assert_eq!(
            members.get(&NameId::from("$var")).unwrap(),
            &DeclarationId::from("$var")
        );
        assert_eq!(
            members.get(&NameId::from("$var2")).unwrap(),
            &DeclarationId::from("$var2")
        );
        assert_eq!(
            members.get(&NameId::from("$var3")).unwrap(),
            &DeclarationId::from("$var3")
        );
    }

    #[test]
    fn class_variable_members() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            @@ignored = 1
            class Foo
              @@var1 = 1
              @@var2, @@var3 = 2

              class << self
                @@var4 = 2
              end
            end
            "
        });

        let declarations = context.graph.declarations();
        // Foo + 5 class variables + <main>, but not @@ignored
        assert_eq!(declarations.len(), 6);
        let main = declarations.get(&DeclarationId::from("Foo")).unwrap();
        let members = main.members();
        assert_eq!(members.len(), 4);
        assert_eq!(
            members.get(&NameId::from("@@var1")).unwrap(),
            &DeclarationId::from("Foo::@@var1")
        );
        assert_eq!(
            members.get(&NameId::from("@@var2")).unwrap(),
            &DeclarationId::from("Foo::@@var2")
        );
        assert_eq!(
            members.get(&NameId::from("@@var3")).unwrap(),
            &DeclarationId::from("Foo::@@var3")
        );
        assert_eq!(
            members.get(&NameId::from("@@var4")).unwrap(),
            &DeclarationId::from("Foo::@@var4")
        );
    }

    #[test]
    fn attribute_members() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              attr_reader :a
              attr_accessor :b
              attr_writer :c
              attr :d
              attr :e, true
              attr :f, :g
            end
            "
        });

        let foo = context.graph.declarations().get(&DeclarationId::from("Foo")).unwrap();
        let members = foo.members();
        assert_eq!(members.len(), 9);
        assert_eq!(members.get(&NameId::from("a")).unwrap(), &DeclarationId::from("Foo::a"));
        assert_eq!(members.get(&NameId::from("b")).unwrap(), &DeclarationId::from("Foo::b"));
        assert_eq!(
            members.get(&NameId::from("b=")).unwrap(),
            &DeclarationId::from("Foo::b=")
        );
        assert_eq!(
            members.get(&NameId::from("c=")).unwrap(),
            &DeclarationId::from("Foo::c=")
        );
        assert_eq!(members.get(&NameId::from("d")).unwrap(), &DeclarationId::from("Foo::d"));
        assert_eq!(members.get(&NameId::from("e")).unwrap(), &DeclarationId::from("Foo::e"));
        assert_eq!(
            members.get(&NameId::from("e=")).unwrap(),
            &DeclarationId::from("Foo::e=")
        );
        assert_eq!(members.get(&NameId::from("f")).unwrap(), &DeclarationId::from("Foo::f"));
        assert_eq!(members.get(&NameId::from("g")).unwrap(), &DeclarationId::from("Foo::g"));
    }

    #[test]
    fn index_unresolved_constant_references_inside_compact_namespace() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar::Baz
                String
              end
            end
            "
        });

        let refs = context.graph.unresolved_references().values().collect::<Vec<_>>();
        assert_eq!(refs.len(), 2);

        let reference = &refs[0];

        match reference {
            UnresolvedReference::Constant(unresolved) => {
                assert_eq!(unresolved.name_id(), &NameId::from("Bar"));
                assert_eq!(
                    unresolved.nesting().as_ref().unwrap().ids_as_vec(),
                    vec![DeclarationId::from("Foo"), DeclarationId::from("Foo::Bar::Baz")]
                );
                assert_eq!(unresolved.uri_id(), UriId::from("file:///foo.rb"));
                assert_eq!(unresolved.offset(), &Offset::new(19, 22));
            }
            UnresolvedReference::Method(unresolved) => {
                panic!("Expected UnresolvedReference::Constant, got {unresolved:?}")
            }
        }

        let reference = &refs[1];

        match reference {
            UnresolvedReference::Constant(unresolved) => {
                assert_eq!(unresolved.name_id(), &NameId::from("String"));
                assert_eq!(
                    unresolved.nesting().as_ref().unwrap().ids_as_vec(),
                    vec![DeclarationId::from("Foo"), DeclarationId::from("Foo::Bar::Baz")]
                );
                assert_eq!(unresolved.uri_id(), UriId::from("file:///foo.rb"));
                assert_eq!(unresolved.offset(), &Offset::new(32, 38));
            }
            UnresolvedReference::Method(unresolved) => {
                panic!("Expected UnresolvedReference::Constant, got {unresolved:?}")
            }
        }
    }

    #[test]
    fn index_unresolved_constant_reference_context() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
                String
              end
            end
            "
        });

        let refs = context.graph.unresolved_references().values().collect::<Vec<_>>();
        assert_eq!(refs.len(), 1);

        let reference = &refs[0];

        match reference {
            UnresolvedReference::Constant(unresolved) => {
                assert_eq!(unresolved.name_id(), &NameId::from("String"));
                assert_eq!(
                    unresolved.nesting().as_ref().unwrap().ids_as_vec(),
                    vec![DeclarationId::from("Foo"), DeclarationId::from("Foo::Bar")]
                );
                assert_eq!(unresolved.uri_id(), UriId::from("file:///foo.rb"));
                assert_eq!(unresolved.offset(), &Offset::new(27, 33));
            }
            UnresolvedReference::Method(unresolved) => {
                panic!("Expected UnresolvedReference::Constant, got {unresolved:?}")
            }
        }
    }

    #[test]
    fn index_unresolved_constant_path_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
                Object::String
              end
            end
            "
        });

        let refs = context.graph.unresolved_references().values().collect::<Vec<_>>();
        assert_eq!(refs.len(), 2);

        let reference = &refs[0];

        match reference {
            UnresolvedReference::Constant(unresolved) => {
                assert_eq!(unresolved.name_id(), &NameId::from("Object"));
                assert_eq!(
                    unresolved.nesting().as_ref().unwrap().ids_as_vec(),
                    vec![DeclarationId::from("Foo"), DeclarationId::from("Foo::Bar")]
                );
                assert_eq!(unresolved.uri_id(), UriId::from("file:///foo.rb"));
                assert_eq!(unresolved.offset(), &Offset::new(27, 33));
            }
            UnresolvedReference::Method(unresolved) => {
                panic!("Expected UnresolvedReference::Constant, got {unresolved:?}")
            }
        }

        let reference = &refs[1];

        match reference {
            UnresolvedReference::Constant(unresolved) => {
                assert_eq!(unresolved.name_id(), &NameId::from("Object::String"));
                assert_eq!(
                    unresolved.nesting().as_ref().unwrap().ids_as_vec(),
                    vec![DeclarationId::from("Foo"), DeclarationId::from("Foo::Bar")]
                );
                assert_eq!(unresolved.uri_id(), UriId::from("file:///foo.rb"));
                assert_eq!(unresolved.offset(), &Offset::new(27, 41));
            }
            UnresolvedReference::Method(unresolved) => {
                panic!("Expected UnresolvedReference::Constant, got {unresolved:?}")
            }
        }
    }

    #[test]
    fn index_unresolved_constant_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            puts C1
            puts C2::C3::C4
            puts foo::C5
            puts C6.foo
            foo = C7
            C8 << 42
            C9 += 42
            C10 ||= 42
            C11 &&= 42
            C12[C13]
            C14::IGNORED1 = 42 # IGNORED1 is an assignment
            C15::C16 << 42
            C17::C18 += 42
            C19::C20 ||= 42
            C21::C22 &&= 42
            puts \"#{C23}\"

            ::IGNORED2 = 42 # IGNORED2 is an assignment
            puts \"IGNORED3\"
            puts :IGNORED4
            "
        });

        assert_eq!(
            collect_constant_reference_names(&context.graph),
            vec![
                "C1",
                "C2",
                "C2::C3",
                "C2::C3::C4",
                "foo::C5",
                "C6",
                "C7",
                "C8",
                "C9",
                "C10",
                "C11",
                "C12",
                "C13",
                "C14",
                "C15",
                "C15::C16",
                "C17",
                "C17::C18",
                "C19",
                "C19::C20",
                "C21",
                "C21::C22",
                "C23"
            ]
        );
    }

    #[test]
    fn index_unresolved_constant_references_from_values() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            IGNORED1 = C1
            IGNORED2 = [C2::C3]
            C4 << C5
            C6 += C7
            C8 ||= C9
            C10 &&= C11
            C12[C13] = C14
            "
        });

        assert_eq!(
            collect_constant_reference_names(&context.graph),
            vec![
                "C1", "C2", "C2::C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14",
            ]
        );
    }

    #[test]
    fn index_unresolved_constant_references_for_classes() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            C1.new

            class IGNORED < ::C2; end
            class IGNORED < C3; end
            class IGNORED < C4::C5; end
            class IGNORED < ::C6::C7; end

            class C8::IGNORED; end
            class ::C9::IGNORED; end
            class C10::C11::IGNORED; end
            "
        });

        assert_eq!(
            collect_constant_reference_names(&context.graph),
            vec![
                "C1", "C2", "C3", "C4", "C4::C5", "C6", "C6::C7", "C8", "C9", "C10", "C10::C11",
            ]
        );
    }

    #[test]
    fn index_unresolved_constant_references_for_modules() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            module X
              include M1
              include M2::M3
              extend M4
              extend M5::M6
              prepend M7
              prepend M8::M9
            end

            M10.include M11
            M12.extend M13
            M14.prepend M15

            module M16::IGNORED; end
            module ::M17::IGNORED; end
            module M18::M19::IGNORED; end
            "
        });

        assert_eq!(
            collect_constant_reference_names(&context.graph),
            vec![
                "M1", "M2", "M2::M3", "M4", "M5", "M5::M6", "M7", "M8", "M8::M9", "M10", "M11", "M12", "M13", "M14",
                "M15", "M16", "M17", "M18", "M18::M19",
            ]
        );
    }

    #[test]
    fn index_unresolved_method_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            m1
            m2(m3)
            m4 m5
            self.m6
            self.m7(m8)
            self.m9 m10
            C.m11
            C.m12(m13)
            C.m14 m15
            m16.m17
            m18.m19(m20)
            m21.m22 m23

            m24.m25.m26

            !m27 # The `!` is collected and will count as one more reference
            m28&.m29
            m30(&m31)
            m32 { m33 }
            m34 do m35 end
            m36[m37] # The `[]` is collected and will count as one more reference

            def foo(&block)
                m38(&block)
            end

            m39(&:m40)
            m41(&m42)
            m43(m44, &m45(m46))
            m47(x: m48, m49:)
            m50(...)
            "
        });

        assert_eq!(
            collect_method_reference_names(&context.graph),
            vec![
                "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16",
                "m17", "m18", "m19", "m20", "m21", "m22", "m23", "m24", "m25", "m26", "!", "m27", "m28", "m29", "m30",
                "m31", "m32", "m33", "m34", "m35", "m36", "[]", "m37", "m38", "m39", "m40", "m41", "m42", "m43", "m44",
                "m45", "m46", "m47", "m48", "m49", "m50"
            ]
        );
    }

    #[test]
    fn index_unresolved_method_assign_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            self.m1 = m2
            m3.m4.m5 = m6.m7.m8
            self.m9, self.m10 = m11, m12
            "
        });

        assert_eq!(
            collect_method_reference_names(&context.graph),
            vec![
                "m1=", "m2", "m3", "m4", "m5=", "m6", "m7", "m8", "m9=", "m10=", "m11", "m12"
            ]
        );
    }

    #[test]
    fn index_unresolved_method_opassign_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            self.m1 += 42
            self.m2 |= 42
            self.m3 ||= 42
            self.m4 &&= 42
            m5.m6 += m7
            m8.m9 ||= m10
            m11.m12 &&= m13
            "
        });

        assert_eq!(
            collect_method_reference_names(&context.graph),
            vec![
                "m1", "m1=", "m2", "m2=", "m3", "m3=", "m4", "m4=", "m5", "m6", "m6=", "m7", "m8", "m9", "m9=", "m10",
                "m11", "m12", "m12=", "m13",
            ]
        );
    }

    #[test]
    fn index_unresolved_method_operator_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            x != x
            x % x
            x & x
            x && x
            x * x
            x ** x
            x + x
            x - x
            x / x
            x << x
            x == x
            x === x
            x >> x
            x ^ x
            x | x
            x || x
            x <=> x
            "
        });

        let mut names = collect_method_reference_names(&context.graph)
            .into_iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();

        names.sort();

        assert_eq!(
            names,
            vec![
                "!=", "%", "&", "&&", "*", "**", "+", "-", "/", "<<", "<=>", "==", "===", ">>", "^", "x", "|", "||",
            ]
        );
    }

    #[test]
    fn index_unresolved_method_lesser_than_operator_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            x < y
            "
        });

        assert_eq!(
            collect_method_reference_names(&context.graph),
            vec!["x", "<", "<=>", "y"]
        );
    }

    #[test]
    fn index_unresolved_method_lesser_than_or_equal_to_operator_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            x <= y
            "
        });

        assert_eq!(
            collect_method_reference_names(&context.graph),
            vec!["x", "<=", "<=>", "y"]
        );
    }

    #[test]
    fn index_unresolved_method_greater_than_operator_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            x > y
            "
        });

        assert_eq!(
            collect_method_reference_names(&context.graph),
            vec!["x", "<=>", ">", "y"]
        );
    }

    #[test]
    fn index_unresolved_method_greater_than_or_equal_to_operator_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            x >= y
            "
        });

        assert_eq!(
            collect_method_reference_names(&context.graph),
            vec!["x", "<=>", ">=", "y"]
        );
    }

    #[test]
    fn index_unresolved_method_alias_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            alias ignored m1
            alias_method :ignored, :m2
            alias_method :ignored, ignored
            "
        });

        assert_eq!(collect_method_reference_names(&context.graph), vec!["m1", "m2"]);
    }
}
