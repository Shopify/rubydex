//! Visit the Ruby AST and create the definitions.

use std::sync::Arc;

use crate::indexing::errors::IndexingError;
use crate::indexing::scope::Scope;
use crate::model::definitions::{
    AttrAccessorDefinition, AttrReaderDefinition, AttrWriterDefinition, ClassDefinition, ClassVariableDefinition,
    ConstantDefinition, Definition, GlobalVariableDefinition, InstanceVariableDefinition, MethodDefinition,
    ModuleDefinition, Parameter, ParameterStruct,
};
use crate::model::graph::Graph;
use crate::model::ids::{DeclarationId, UriId};
use crate::model::references::{ConstantReference, UnresolvedReference};
use crate::offset::Offset;
use crate::source_location::SourceLocationConverter;

use ruby_prism::{ParseResult, Visit};

pub type IndexerParts = (Option<Graph>, Vec<IndexingError>);

const MAGIC_AND_RBS_COMMENT_PREFIX: &[&str] = &[
    "frozen_string_literal:",
    "typed:",
    "compiled:",
    "encoding:",
    "shareable_constant_value:",
    "warn_indent:",
    "rubocop:",
    "nodoc:",
    "doc:",
    "coding:",
    "warn_past_scope:",
    "#:",
    "#|",
];

/// The indexer for the definitions found in the Ruby source code.
///
/// It implements the `Visit` trait from `ruby_prism` to visit the AST and create a hash of definitions that must be
/// merged into the global state later.
pub struct RubyIndexer<'a> {
    uri_id: UriId,
    local_graph: Graph,
    errors: Vec<IndexingError>,
    location_converter: &'a dyn SourceLocationConverter,
    comments: Vec<CommentGroup>,
    source: &'a str,
    scope: Scope,
}

impl<'a> RubyIndexer<'a> {
    #[must_use]
    pub fn new(
        uri: String,
        location_converter: &'a dyn SourceLocationConverter,
        source: &'a str,
        content_hash: u16,
    ) -> Self {
        let mut local_index = Graph::new();
        let uri_id = local_index.add_uri(uri, content_hash);

        Self {
            uri_id,
            local_graph: local_index,
            errors: Vec::new(),
            location_converter,
            comments: Vec::new(),
            source,
            scope: Scope::new(),
        }
    }

    #[must_use]
    pub fn into_parts(self) -> IndexerParts {
        (Some(self.local_graph), self.errors)
    }

    pub fn add_error(&mut self, error: IndexingError) {
        self.errors.push(error);
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

    fn find_comments_for(&self, offset: u32) -> Option<String> {
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
}

struct CommentGroup {
    end_offset: usize,
    comments: String,
}

impl CommentGroup {
    #[must_use]
    pub fn new() -> Self {
        Self {
            end_offset: 0,
            comments: String::new(),
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
        if MAGIC_AND_RBS_COMMENT_PREFIX.iter().any(|&prefix| text.contains(prefix)) {
            return;
        }

        let parsed_comment = text.trim().strip_prefix("# ").unwrap_or(text.trim());
        if !self.comments.is_empty() {
            self.comments.push('\n');
        }
        self.comments.push_str(parsed_comment);
    }
}

impl Visit<'_> for RubyIndexer<'_> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'_>) {
        let name = Self::location_to_string(&node.constant_path().location());
        let (fully_qualified_name, declaration_id) = self.scope.enter(&name);
        let offset = Offset::from_prism_location(&node.location());
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();

        // Uses `location_converter` to evaluate the performance impact of the conversion
        self.location_converter.offset_to_position(&offset).unwrap();

        let definition = Definition::Class(Box::new(ClassDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));

        self.local_graph.add_definition(fully_qualified_name, definition);

        if let Some(body) = node.body() {
            self.visit(&body);
        }

        self.scope.leave();
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode) {
        let name = Self::location_to_string(&node.constant_path().location());
        let (fully_qualified_name, declaration_id) = self.scope.enter(&name);
        let offset = Offset::from_prism_location(&node.location());
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();

        // Uses `location_converter` to evaluate the performance impact of the conversion
        self.location_converter.offset_to_position(&offset).unwrap();
        let definition = Definition::Module(Box::new(ModuleDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));
        self.local_graph.add_definition(fully_qualified_name, definition);

        if let Some(body) = node.body() {
            self.visit(&body);
        }

        self.scope.leave();
    }

    fn visit_constant_write_node(&mut self, node: &ruby_prism::ConstantWriteNode) {
        let name_loc = node.name_loc();
        let name = Self::location_to_string(&name_loc);
        let fully_qualified_name = self.scope.fully_qualify(&name);

        let declaration_id = DeclarationId::from(&fully_qualified_name);
        let offset = Offset::from_prism_location(&name_loc);
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();
        // Uses `location_converter` to evaluate the performance impact of the conversion
        self.location_converter.offset_to_position(&offset).unwrap();
        let definition = Definition::Constant(Box::new(ConstantDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));

        self.local_graph.add_definition(fully_qualified_name, definition);
        self.visit(&node.value());
    }

    fn visit_constant_path_write_node(&mut self, node: &ruby_prism::ConstantPathWriteNode) {
        let location = node.target().location();
        let name = Self::location_to_string(&location);
        let fully_qualified_name = self.scope.fully_qualify(&name);

        let declaration_id = DeclarationId::from(&fully_qualified_name);
        let offset = Offset::from_prism_location(&location);
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();
        // Uses `location_converter` to evaluate the performance impact of the conversion
        self.location_converter.offset_to_position(&offset).unwrap();
        let definition = Definition::Constant(Box::new(ConstantDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));

        self.local_graph.add_definition(fully_qualified_name, definition);
        self.visit(&node.value());
    }

    fn visit_constant_read_node(&mut self, node: &ruby_prism::ConstantReadNode<'_>) {
        self.index_constant_reference(&node.location());
    }

    fn visit_constant_path_node(&mut self, node: &ruby_prism::ConstantPathNode<'_>) {
        self.index_constant_reference(&node.location());
    }

    fn visit_multi_write_node(&mut self, node: &ruby_prism::MultiWriteNode) {
        for left in node.lefts().iter() {
            match left {
                ruby_prism::Node::ConstantTargetNode { .. } | ruby_prism::Node::ConstantPathTargetNode { .. } => {
                    let location = left.location();
                    let name = Self::location_to_string(&location);
                    let fully_qualified_name = self.scope.fully_qualify(&name);

                    let declaration_id = DeclarationId::from(&fully_qualified_name);
                    let offset = Offset::from_prism_location(&location);
                    let comments = self.find_comments_for(offset.start()).unwrap_or_default();
                    // Uses `location_converter` to evaluate the performance impact of the conversion
                    self.location_converter.offset_to_position(&offset).unwrap();

                    let definition = Definition::Constant(Box::new(ConstantDefinition::new(
                        declaration_id,
                        self.uri_id,
                        offset,
                        comments,
                    )));

                    self.local_graph.add_definition(fully_qualified_name, definition);
                }
                ruby_prism::Node::GlobalVariableTargetNode { .. } => {
                    let location = left.location();
                    let name = Self::location_to_string(&location);
                    let declaration_id = DeclarationId::from(&name);
                    let offset = Offset::from_prism_location(&location);
                    // Uses `location_converter` to evaluate the performance impact of the conversion
                    self.location_converter.offset_to_position(&offset).unwrap();

                    let comments = self.find_comments_for(offset.start()).unwrap_or_default();
                    let definition = Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                        declaration_id,
                        self.uri_id,
                        offset,
                        comments,
                    )));

                    self.local_graph.add_definition(name, definition);
                }
                ruby_prism::Node::InstanceVariableTargetNode { .. } => {
                    let location = left.location();
                    let name = Self::location_to_string(&location);
                    let fully_qualified_name = self.scope.fully_qualify(&name);

                    let declaration_id = DeclarationId::from(&fully_qualified_name);
                    let offset = Offset::from_prism_location(&location);
                    let comments = self.find_comments_for(offset.start()).unwrap_or_default();
                    // Uses `location_converter` to evaluate the performance impact of the conversion
                    self.location_converter.offset_to_position(&offset).unwrap();

                    let definition = Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
                        declaration_id,
                        self.uri_id,
                        offset,
                        comments,
                    )));

                    self.local_graph.add_definition(fully_qualified_name, definition);
                }
                ruby_prism::Node::ClassVariableTargetNode { .. } => {
                    let location = left.location();
                    let name = Self::location_to_string(&location);
                    let fully_qualified_name = self.scope.fully_qualify(&name);

                    let declaration_id = DeclarationId::from(&fully_qualified_name);
                    let offset = Offset::from_prism_location(&location);
                    let comments = self.find_comments_for(offset.start()).unwrap_or_default();
                    // Uses `location_converter` to evaluate the performance impact of the conversion
                    self.location_converter.offset_to_position(&offset).unwrap();

                    let definition = Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
                        declaration_id,
                        self.uri_id,
                        offset,
                        comments,
                    )));

                    self.local_graph.add_definition(fully_qualified_name, definition);
                }
                _ => {}
            }
        }

        self.visit(&node.value());
    }

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode) {
        let name = Self::location_to_string(&node.name_loc());
        let fully_qualified_name = self.scope.fully_qualify(&name);

        let declaration_id = DeclarationId::from(&fully_qualified_name);
        let offset = Offset::from_prism_location(&node.location());
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();
        // Uses `location_converter` to evaluate the performance impact of the conversion
        self.location_converter.offset_to_position(&offset).unwrap();

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
            .add_definition(fully_qualified_name, Definition::Method(Box::new(method)));

        if let Some(body) = node.body() {
            self.visit(&body);
        }
    }

    #[allow(clippy::too_many_lines)]
    fn visit_call_node(&mut self, node: &ruby_prism::CallNode) {
        fn create_attr_accessor(indexer: &mut RubyIndexer, node: &ruby_prism::CallNode) {
            RubyIndexer::each_string_or_symbol_arg(node, |name, location| {
                let fully_qualified_name = indexer.scope.fully_qualify(&name);
                let declaration_id = DeclarationId::from(&fully_qualified_name);
                let writer_name = format!("{fully_qualified_name}=");
                let writer_declaration_id = DeclarationId::from(&writer_name);
                let offset = Offset::from_prism_location(&location);
                let comments = indexer.find_comments_for(offset.start()).unwrap_or_default();
                // Uses `location_converter` to evaluate the performance impact of the conversion
                indexer.location_converter.offset_to_position(&offset).unwrap();
                indexer.local_graph.add_definition(
                    fully_qualified_name,
                    Definition::AttrAccessor(Box::new(AttrAccessorDefinition::new(
                        declaration_id,
                        indexer.uri_id,
                        offset,
                        comments.clone(),
                    ))),
                );

                indexer.local_graph.add_definition(
                    writer_name,
                    Definition::AttrAccessor(Box::new(AttrAccessorDefinition::new(
                        writer_declaration_id,
                        indexer.uri_id,
                        Offset::from_prism_location(&location),
                        comments.clone(),
                    ))),
                );
            });
        }

        fn create_attr_reader(indexer: &mut RubyIndexer, node: &ruby_prism::CallNode) {
            RubyIndexer::each_string_or_symbol_arg(node, |name, location| {
                let fully_qualified_name = indexer.scope.fully_qualify(&name);
                let declaration_id = DeclarationId::from(&fully_qualified_name);
                let offset = Offset::from_prism_location(&location);
                let comments = indexer.find_comments_for(offset.start()).unwrap_or_default();
                // Uses `location_converter` to evaluate the performance impact of the conversion
                indexer.location_converter.offset_to_position(&offset).unwrap();

                indexer.local_graph.add_definition(
                    fully_qualified_name,
                    Definition::AttrReader(Box::new(AttrReaderDefinition::new(
                        declaration_id,
                        indexer.uri_id,
                        offset,
                        comments,
                    ))),
                );
            });
        }

        fn create_attr_writer(indexer: &mut RubyIndexer, node: &ruby_prism::CallNode) {
            RubyIndexer::each_string_or_symbol_arg(node, |name, location| {
                let fully_qualified_name = indexer.scope.fully_qualify(&name);
                let writer_name = format!("{fully_qualified_name}=");
                let declaration_id = DeclarationId::from(&writer_name);
                let offset = Offset::from_prism_location(&location);
                let comments = indexer.find_comments_for(offset.start()).unwrap_or_default();
                // Uses `location_converter` to evaluate the performance impact of the conversion
                indexer.location_converter.offset_to_position(&offset).unwrap();

                indexer.local_graph.add_definition(
                    writer_name,
                    Definition::AttrWriter(Box::new(AttrWriterDefinition::new(
                        declaration_id,
                        indexer.uri_id,
                        offset,
                        comments,
                    ))),
                );
            });
        }

        let message_loc = node.message_loc();

        if message_loc.is_none() {
            // No message, we can't index this node
            return;
        }

        let message = Self::location_to_string(&message_loc.unwrap());

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
                // attr :foo, "bar", :baz => both reader and writer for foo, bar, and baz
                let create_writer = if let Some(arguments) = node.arguments() {
                    let args_vec: Vec<_> = arguments.arguments().iter().collect();

                    matches!(
                        args_vec.as_slice(),
                        [_, ruby_prism::Node::TrueNode { .. }]
                            | [
                                _,
                                ruby_prism::Node::SymbolNode { .. } | ruby_prism::Node::StringNode { .. },
                                ..,
                            ]
                    )
                } else {
                    false
                };

                if create_writer {
                    create_attr_accessor(self, node);
                } else {
                    create_attr_reader(self, node);
                }
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
        let declaration_id = DeclarationId::from(&name);
        let offset = Offset::from_prism_location(&name_loc);

        let comments = self.find_comments_for(offset.start()).unwrap_or_default();
        let definition = Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));

        self.local_graph.add_definition(name, definition);

        self.visit(&node.value());
    }

    fn visit_instance_variable_write_node(&mut self, node: &ruby_prism::InstanceVariableWriteNode) {
        let name_loc = node.name_loc();
        let name = Self::location_to_string(&name_loc);
        let fully_qualified_name = self.scope.fully_qualify(&name);

        let declaration_id = DeclarationId::from(&fully_qualified_name);
        let offset = Offset::from_prism_location(&name_loc);
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();
        // Uses `location_converter` to evaluate the performance impact of the conversion
        self.location_converter.offset_to_position(&offset).unwrap();

        let definition = Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));

        self.local_graph.add_definition(fully_qualified_name, definition);
        self.visit(&node.value());
    }

    fn visit_class_variable_write_node(&mut self, node: &ruby_prism::ClassVariableWriteNode) {
        let name_loc = node.name_loc();
        let name = Self::location_to_string(&name_loc);
        let fully_qualified_name = self.scope.fully_qualify(&name);

        let declaration_id = DeclarationId::from(&fully_qualified_name);
        let offset = Offset::from_prism_location(&name_loc);
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();
        // Uses `location_converter` to evaluate the performance impact of the conversion
        self.location_converter.offset_to_position(&offset).unwrap();

        let definition = Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
            declaration_id,
            self.uri_id,
            offset,
            comments,
        )));

        self.local_graph.add_definition(fully_qualified_name, definition);
        self.visit(&node.value());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::GraphTest;

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
            assert_definition!($context, $variant, $name, |def: &Definition| {
                assert_eq!(def.comments(), $expected_comments);
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

        let definitions = context.graph.get("Foo::Bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 12, 46);

        let definitions = context.graph.get("Foo::Bar::Baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 26, 40);

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

        let definitions = context.graph.get("Foo::Bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 13, 49);

        let definitions = context.graph.get("Foo::Bar::Baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 28, 43);

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

        let definitions = context.graph.get("Foo::FOO").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 21, 24);
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
        assert_definition_offset!(definitions[0], 15, 18);

        let definitions = context.graph.get("foo=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 15, 18);

        let definitions = context.graph.get("Foo::bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 47, 50);

        let definitions = context.graph.get("Foo::bar=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 47, 50);

        let definitions = context.graph.get("Foo::baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 53, 56);

        let definitions = context.graph.get("Foo::baz=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 53, 56);

        let definitions = context.graph.get("qux").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 82, 85);

        let definitions = context.graph.get("qux=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 82, 85);

        assert!(context.graph.get("not_indexed").is_none());
        assert!(context.graph.get("not_indexed=").is_none());
    }

    #[test]
    fn comment_basic_attachment() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            # Single comment
            class Single; end

            # Multi-line comment 1
            # Multi-line comment 2
            # Multi-line comment 3
            class Multi; end

            # Comment directly above (no gap)
            class NoGap; end

            # Comment with blank line

            class BlankLine; end

            class NoComment; end
            "
        });

        assert_definition_comments!(context, Class, "Single", "Single comment");
        assert_definition_comments!(
            context,
            Class,
            "Multi",
            "Multi-line comment 1\nMulti-line comment 2\nMulti-line comment 3"
        );
        assert_definition_comments!(context, Class, "NoGap", "Comment directly above (no gap)");
        assert_definition_comments!(context, Class, "BlankLine", "Comment with blank line");
        assert_definition_comments!(context, Class, "NoComment", "");
    }

    #[test]
    fn comment_separation_rules() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            # Too far away


            class TooFar; end

            # Not attached due to code
            x = 1
            class CodeBetween; end

            # Comment for Foo
            class Foo; end
            "
        });

        assert_definition_comments!(context, Class, "TooFar", "");
        assert_definition_comments!(context, Class, "CodeBetween", "");
        assert_definition_comments!(context, Class, "Foo", "Comment for Foo");
    }

    #[test]
    fn comment_indented_and_nested() {
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

        assert_definition_comments!(context, Class, "Outer", "Outer class");
        assert_definition_comments!(context, Class, "Outer::Inner", "Inner class at 2 spaces");
        assert_definition_comments!(context, Class, "Outer::Inner::Deep", "Deep class at 4 spaces");
        assert_definition_comments!(
            context,
            Class,
            "Outer::AnotherInner",
            "Another inner class\nwith multiple lines"
        );
    }

    #[test]
    fn comment_modules_and_constants() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            # Module comment
            module TestModule
              # Constant comment
              FOO = 42

              # Nested module
              module Nested; end
            end

            # Multi-write constant
            A, B = 1, 2
            "
        });

        assert_definition_comments!(context, Module, "TestModule", "Module comment");
        assert_definition_comments!(context, Constant, "TestModule::FOO", "Constant comment");
        assert_definition_comments!(context, Module, "TestModule::Nested", "Nested module");
        assert_definition_comments!(context, Constant, "A", "Multi-write constant");
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
        assert_definition_offset!(definitions[0], 13, 16);

        assert!(context.graph.get("foo=").is_none());

        let definitions = context.graph.get("Foo::bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 43, 46);

        assert!(context.graph.get("Foo::bar=").is_none());

        let definitions = context.graph.get("Foo::baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 49, 52);

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
        assert_definition_offset!(definitions[0], 13, 16);

        assert!(context.graph.get("foo").is_none());

        let definitions = context.graph.get("Foo::bar=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 43, 46);

        assert!(context.graph.get("Foo::bar").is_none());

        let definitions = context.graph.get("Foo::baz=").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 49, 52);

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
        assert_definition_offset!(definitions[0], 0, 4);

        let definitions = context.graph.get("$bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 9, 13);

        let definitions = context.graph.get("$baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 15, 19);

        let definitions = context.graph.get("$qux").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 40, 44);
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

        let definitions = context.graph.get("Foo::@bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 22, 26);

        let definitions = context.graph.get("Foo::@baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 34, 38);

        let definitions = context.graph.get("Foo::@qux").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 40, 44);
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
        assert_definition_offset!(definitions[0], 0, 5);

        let definitions = context.graph.get("Foo::@@bar").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 23, 28);

        let definitions = context.graph.get("Foo::@@baz").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 36, 41);

        let definitions = context.graph.get("Foo::@@qux").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_definition_offset!(definitions[0], 43, 48);
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
            "Foo::foo",
            "Foo::foo=",
            "Foo::bar",
            "Foo::bar=",
            "Foo::baz",
            "Foo::baz=",
            "Foo::a",
            "Foo::a=",
            "Foo::b",
            "Foo::b=",
            "Foo::c",
            "Foo::c=",
            "Foo::d",
            "Foo::d=",
        ] {
            let definitions = context.graph.get(name).unwrap();
            assert_eq!(definitions.len(), 1);
            assert!(matches!(definitions[0], Definition::AttrAccessor(_)));
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
}
