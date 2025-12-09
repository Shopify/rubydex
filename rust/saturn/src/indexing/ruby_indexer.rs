//! Visit the Ruby AST and create the definitions.

use crate::diagnostic::Severity;
use crate::errors::Errors;
use crate::indexing::local_graph::LocalGraph;
use crate::model::comment::Comment;
use crate::model::definitions::{
    AttrAccessorDefinition, AttrReaderDefinition, AttrWriterDefinition, ClassDefinition, ClassVariableDefinition,
    ConstantDefinition, Definition, GlobalVariableDefinition, InstanceVariableDefinition, MethodDefinition, Mixin,
    ModuleDefinition, Parameter, ParameterStruct, SingletonClassDefinition,
};
use crate::model::document::Document;
use crate::model::ids::{DefinitionId, NameId, StringId, UriId};
use crate::model::name::Name;
use crate::model::references::{ConstantReference, MethodRef};
use crate::model::visibility::Visibility;
use crate::offset::Offset;

use ruby_prism::{ParseResult, Visit};

pub type IndexerParts = (LocalGraph, Vec<Errors>);

#[derive(Clone, Copy)]
enum MixinType {
    Include,
    Prepend,
    Extend,
}

/// The indexer for the definitions found in the Ruby source code.
///
/// It implements the `Visit` trait from `ruby_prism` to visit the AST and create a hash of definitions that must be
/// merged into the global state later.
pub struct RubyIndexer<'a> {
    uri_id: UriId,
    local_graph: LocalGraph,
    source: &'a str,
    comments: Vec<CommentGroup>,
    definitions_stack: Vec<DefinitionId>,
    visibility_stack: Vec<Visibility>,
}

impl<'a> RubyIndexer<'a> {
    #[must_use]
    pub fn new(uri: String, source: &'a str) -> Self {
        let uri_id = UriId::from(&uri);
        let local_graph = LocalGraph::new(uri_id, Document::new(uri));

        Self {
            uri_id,
            local_graph,
            source,
            comments: Vec::new(),
            definitions_stack: Vec::new(),
            visibility_stack: vec![Visibility::Private],
        }
    }

    #[must_use]
    pub fn local_graph(self) -> LocalGraph {
        self.local_graph
    }

    pub fn index(&mut self) {
        let result = ruby_prism::parse(self.source.as_bytes());

        for error in result.errors() {
            self.local_graph.add_diagnostic(
                Offset::from_prism_location(&error.location()),
                error.message().to_string(),
                Severity::Error,
            );
        }

        for warning in result.warnings() {
            self.local_graph.add_diagnostic(
                Offset::from_prism_location(&warning.location()),
                warning.message().to_string(),
                Severity::Warning,
            );
        }

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

    fn collect_parameters(&mut self, node: &ruby_prism::DefNode) -> Vec<Parameter> {
        let mut parameters: Vec<Parameter> = Vec::new();

        if let Some(parameters_list) = node.parameters() {
            for parameter in &parameters_list.requireds() {
                let location = parameter.location();
                let str_id = self.local_graph.intern_string(Self::location_to_string(&location));

                parameters.push(Parameter::RequiredPositional(ParameterStruct::new(
                    Offset::from_prism_location(&location),
                    str_id,
                )));
            }

            for parameter in &parameters_list.optionals() {
                let opt_param = parameter.as_optional_parameter_node().unwrap();
                let name_loc = opt_param.name_loc();
                let str_id = self.local_graph.intern_string(Self::location_to_string(&name_loc));

                parameters.push(Parameter::OptionalPositional(ParameterStruct::new(
                    Offset::from_prism_location(&name_loc),
                    str_id,
                )));
            }

            if let Some(rest) = parameters_list.rest() {
                let rest_param = rest.as_rest_parameter_node().unwrap();
                let location = rest_param.name_loc().unwrap_or_else(|| rest.location());
                let str_id = self.local_graph.intern_string(Self::location_to_string(&location));

                parameters.push(Parameter::RestPositional(ParameterStruct::new(
                    Offset::from_prism_location(&location),
                    str_id,
                )));
            }

            for post in &parameters_list.posts() {
                let location = post.location();
                let str_id = self.local_graph.intern_string(Self::location_to_string(&location));

                parameters.push(Parameter::Post(ParameterStruct::new(
                    Offset::from_prism_location(&location),
                    str_id,
                )));
            }

            for keyword in &parameters_list.keywords() {
                match keyword {
                    ruby_prism::Node::RequiredKeywordParameterNode { .. } => {
                        let required = keyword.as_required_keyword_parameter_node().unwrap();
                        let name_loc = required.name_loc();
                        let str_id = self
                            .local_graph
                            .intern_string(Self::location_to_string(&name_loc).trim_end_matches(':').to_string());

                        parameters.push(Parameter::RequiredKeyword(ParameterStruct::new(
                            Offset::from_prism_location(&name_loc),
                            str_id,
                        )));
                    }
                    ruby_prism::Node::OptionalKeywordParameterNode { .. } => {
                        let optional = keyword.as_optional_keyword_parameter_node().unwrap();
                        let name_loc = optional.name_loc();
                        let str_id = self
                            .local_graph
                            .intern_string(Self::location_to_string(&name_loc).trim_end_matches(':').to_string());

                        parameters.push(Parameter::OptionalKeyword(ParameterStruct::new(
                            Offset::from_prism_location(&name_loc),
                            str_id,
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
                        let str_id = self.local_graph.intern_string(Self::location_to_string(&location));

                        parameters.push(Parameter::RestKeyword(ParameterStruct::new(
                            Offset::from_prism_location(&location),
                            str_id,
                        )));
                    }
                    ruby_prism::Node::ForwardingParameterNode { .. } => {
                        let location = rest.location();
                        let str_id = self.local_graph.intern_string(Self::location_to_string(&location));

                        parameters.push(Parameter::Forward(ParameterStruct::new(
                            Offset::from_prism_location(&location),
                            str_id,
                        )));
                    }
                    _ => {
                        // Do nothing
                    }
                }
            }

            if let Some(block) = parameters_list.block() {
                let location = block.name_loc().unwrap_or_else(|| block.location());
                let str_id = self.local_graph.intern_string(Self::location_to_string(&location));

                parameters.push(Parameter::Block(ParameterStruct::new(
                    Offset::from_prism_location(&location),
                    str_id,
                )));
            }
        }

        parameters
    }

    /// Gets the `NameId` of the current class/module/singleton block.
    /// Used to resolve `self` to a concrete `NameId` during indexing.
    fn current_nesting_name_id(&self) -> Option<NameId> {
        let parent_nesting_id = self.parent_nesting_id()?;
        let definition = self.local_graph.definitions().get(parent_nesting_id)?;

        match definition {
            Definition::Class(class_def) => Some(*class_def.name_id()),
            Definition::Module(module_def) => Some(*module_def.name_id()),
            Definition::SingletonClass(singleton_class_def) => Some(*singleton_class_def.name_id()),
            _ => panic!("current nesting is not a class/module/singleton class"),
        }
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
            for argument in &arguments.arguments() {
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

    fn index_constant_reference(&mut self, node: &ruby_prism::Node, push_final_reference: bool) -> Option<NameId> {
        let mut parent_scope_id = None;
        let mut top_level_ref = false;

        let location = match node {
            ruby_prism::Node::ConstantPathNode { .. } => {
                let constant = node.as_constant_path_node().unwrap();

                if let Some(parent) = constant.parent() {
                    // Ignore parent scopes that are not constants, like `foo::Bar`
                    match parent {
                        ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. } => {}
                        _ => {
                            return None;
                        }
                    }

                    parent_scope_id = self.index_constant_reference(&parent, true);
                } else {
                    top_level_ref = true;
                }

                constant.name_loc()
            }
            ruby_prism::Node::ConstantReadNode { .. } => node.location(),
            ruby_prism::Node::ConstantAndWriteNode { .. } => node.as_constant_and_write_node().unwrap().name_loc(),
            ruby_prism::Node::ConstantOperatorWriteNode { .. } => {
                node.as_constant_operator_write_node().unwrap().name_loc()
            }
            ruby_prism::Node::ConstantOrWriteNode { .. } => node.as_constant_or_write_node().unwrap().name_loc(),
            ruby_prism::Node::ConstantTargetNode { .. } => node.as_constant_target_node().unwrap().location(),
            ruby_prism::Node::ConstantWriteNode { .. } => node.as_constant_write_node().unwrap().name_loc(),
            ruby_prism::Node::ConstantPathTargetNode { .. } => {
                let target = node.as_constant_path_target_node().unwrap();

                if let Some(parent) = target.parent() {
                    match parent {
                        ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. } => {}
                        _ => {
                            return None;
                        }
                    }

                    parent_scope_id = self.index_constant_reference(&parent, true);
                }

                target.name_loc()
            }
            _ => {
                return None;
            }
        };

        let offset = Offset::from_prism_location(&location);
        let name = Self::location_to_string(&location);

        // There are 3 possible cases here:
        //
        //  - A top level reference starting with `::` (if branch)
        //  - A reference inside of a nesting
        //  - A reference outside of any nesting
        let nesting = if top_level_ref {
            None
        } else {
            self.current_nesting_name_id()
        };

        let string_id = self.local_graph.intern_string(name);
        let name_id = self
            .local_graph
            .add_name(Name::new(string_id, parent_scope_id, nesting));

        if push_final_reference {
            self.local_graph
                .add_constant_reference(ConstantReference::new(name_id, self.uri_id, offset));
        }

        Some(name_id)
    }

    fn index_method_reference(&mut self, name: String, location: &ruby_prism::Location) {
        let offset = Offset::from_prism_location(location);
        let str_id = self.local_graph.intern_string(name);
        let reference = MethodRef::new(str_id, self.uri_id, offset);
        self.local_graph.add_method_reference(reference);
    }

    fn add_definition_from_location<F>(&mut self, location: &ruby_prism::Location, builder: F) -> DefinitionId
    where
        F: FnOnce(StringId, Offset, Vec<Comment>, Option<DefinitionId>, UriId) -> Definition,
    {
        let name = Self::location_to_string(location);
        let str_id = self.local_graph.intern_string(name);
        let offset = Offset::from_prism_location(location);
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();
        let lexical_nesting_id = self.parent_nesting_id().copied();
        let uri_id = self.uri_id;

        let definition = builder(str_id, offset, comments, lexical_nesting_id, uri_id);
        let definition_id = self.local_graph.add_definition(definition);

        self.add_member_to_current_nesting(definition_id);

        definition_id
    }

    fn add_constant_definition(&mut self, node: &ruby_prism::Node, also_add_reference: bool) -> Option<DefinitionId> {
        let name_id = self.index_constant_reference(node, also_add_reference)?;

        // Get the location for the constant name/path only (not including the value)
        let location = match node {
            ruby_prism::Node::ConstantWriteNode { .. } => node.as_constant_write_node().unwrap().name_loc(),
            ruby_prism::Node::ConstantOrWriteNode { .. } => node.as_constant_or_write_node().unwrap().name_loc(),
            _ => node.location(),
        };

        let offset = Offset::from_prism_location(&location);
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();
        let lexical_nesting_id = self.parent_nesting_id().copied();

        let definition = Definition::Constant(Box::new(ConstantDefinition::new(
            name_id,
            self.uri_id,
            offset,
            comments,
            lexical_nesting_id,
        )));
        let definition_id = self.local_graph.add_definition(definition);

        self.add_member_to_current_nesting(definition_id);

        Some(definition_id)
    }

    /// Adds a member to the current nesting definition.
    ///
    /// # Panics
    ///
    /// Panics if the definition is not a nesting definition (class, module, or singleton class)
    fn add_member_to_current_nesting(&mut self, member_id: DefinitionId) {
        let Some(parent_nesting) = self.parent_nesting() else {
            return;
        };

        match parent_nesting {
            Definition::Class(class) => class.add_member(member_id),
            Definition::SingletonClass(singleton_class) => singleton_class.add_member(member_id),
            Definition::Module(module) => module.add_member(member_id),
            _ => panic!("Cannot add a member to a non-nesting definition (current nesting is a {parent_nesting:?})",),
        }
    }

    /// Returns the name ID of this definition
    ///
    /// # Panics
    ///
    /// Panics if the definition is not a nesting definition (class, module, or singleton class)
    #[must_use]
    fn parent_nesting_name_id(&mut self) -> Option<NameId> {
        let parent_nesting = self.parent_nesting()?;

        match parent_nesting {
            Definition::Class(class) => Some(*class.name_id()),
            Definition::SingletonClass(singleton_class) => Some(*singleton_class.name_id()),
            Definition::Module(module) => Some(*module.name_id()),
            Definition::Constant(constant) => Some(*constant.name_id()),
            _ => panic!("Cannot get name ID of definition: {parent_nesting:?}"),
        }
    }

    fn handle_mixin(&mut self, node: &ruby_prism::CallNode, mixin_type: MixinType) {
        let Some(arguments) = node.arguments() else {
            return;
        };

        // Collect all arguments as constant references. Ignore anything that isn't a constant
        let reference_ids: Vec<_> = arguments
            .arguments()
            .iter()
            .filter_map(|arg| {
                if arg.as_self_node().is_some() {
                    // FIXME: Ideally we would want to save the mixin as `self` but we can only save mixins with a name.
                    // We'll just use the parent nesting name id for now.
                    self.parent_nesting_name_id()
                } else {
                    self.index_constant_reference(&arg, true)
                }
            })
            .collect();

        if reference_ids.is_empty() {
            return;
        }

        let Some(lexical_nesting_id) = self.parent_nesting_id().copied() else {
            return;
        };
        let Some(definition) = self.local_graph.get_definition_mut(lexical_nesting_id) else {
            return;
        };

        for id in reference_ids {
            let mixin = match mixin_type {
                MixinType::Include => Mixin::Include(id),
                MixinType::Prepend => Mixin::Prepend(id),
                MixinType::Extend => Mixin::Extend(id),
            };

            match definition {
                Definition::Class(class_def) => class_def.add_mixin(mixin),
                Definition::Module(module_def) => module_def.add_mixin(mixin),
                Definition::SingletonClass(singleton_class_def) => singleton_class_def.add_mixin(mixin),
                _ => {}
            }
        }
    }

    /// Visits every part of a call node, except for the message itself. Convenient for when we're only interested in
    /// continuing the traversal
    fn visit_call_node_parts(&mut self, node: &ruby_prism::CallNode) {
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

    #[must_use]
    fn parent_nesting_id(&self) -> Option<&DefinitionId> {
        self.definitions_stack.last()
    }

    #[must_use]
    fn parent_nesting(&mut self) -> Option<&mut Definition> {
        let parent_id = self.parent_nesting_id()?;
        self.local_graph.get_definition_mut(*parent_id)
    }

    #[must_use]
    fn current_visibility(&self) -> &Visibility {
        self.visibility_stack.last().unwrap()
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
        let offset = Offset::from_prism_location(&node.location());
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();
        let lexical_nesting_id = self.parent_nesting_id().copied();
        let superclass = node.superclass().and_then(|n| self.index_constant_reference(&n, true));

        if let Some(name_id) = self.index_constant_reference(&node.constant_path(), false) {
            let definition = Definition::Class(Box::new(ClassDefinition::new(
                name_id,
                self.uri_id,
                offset,
                comments,
                lexical_nesting_id,
                superclass,
            )));

            let definition_id = self.local_graph.add_definition(definition);

            self.add_member_to_current_nesting(definition_id);

            self.definitions_stack.push(definition_id);
            self.visibility_stack.push(Visibility::Public);

            if let Some(body) = node.body() {
                self.visit(&body);
            }

            self.visibility_stack.pop();
            self.definitions_stack.pop();
        }
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode) {
        let offset = Offset::from_prism_location(&node.location());
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();
        let lexical_nesting_id = self.parent_nesting_id().copied();

        if let Some(constant_ref_id) = self.index_constant_reference(&node.constant_path(), false) {
            let definition = Definition::Module(Box::new(ModuleDefinition::new(
                constant_ref_id,
                self.uri_id,
                offset,
                comments,
                lexical_nesting_id,
            )));

            let definition_id = self.local_graph.add_definition(definition);

            self.add_member_to_current_nesting(definition_id);

            self.definitions_stack.push(definition_id);
            self.visibility_stack.push(Visibility::Public);

            if let Some(body) = node.body() {
                self.visit(&body);
            }

            self.visibility_stack.pop();
            self.definitions_stack.pop();
        }
    }

    fn visit_singleton_class_node(&mut self, node: &ruby_prism::SingletonClassNode) {
        let expression = node.expression();

        // Determine the attached_target for the singleton class
        let attached_target = if expression.as_self_node().is_some() {
            // `class << self` - resolve self to current class/module's NameId
            self.current_nesting_name_id()
        } else if matches!(
            expression,
            ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. }
        ) {
            // `class << Foo` or `class << Foo::Bar` - use the constant's NameId
            self.index_constant_reference(&expression, true)
        } else {
            // Dynamic expression (e.g., `class << some_var`) - skip creating definition
            self.visit(&expression);
            None
        };

        if attached_target.is_none() {
            return;
        }

        // Create SingletonClassDefinition if we have an attached_target
        let definition_id = if let Some(attached_target) = attached_target {
            let offset = Offset::from_prism_location(&node.location());
            let comments = self.find_comments_for(offset.start()).unwrap_or_default();
            let lexical_nesting_id = self.parent_nesting_id().copied();

            let singleton_class_name = {
                let name = self
                    .local_graph
                    .names()
                    .get(&attached_target)
                    .expect("Attached target name should exist");
                let target_str = self
                    .local_graph
                    .strings()
                    .get(name.str())
                    .expect("Attached target string should exist");
                format!("<{target_str}>")
            };

            let string_id = self.local_graph.intern_string(singleton_class_name);
            let name_id = self
                .local_graph
                .add_name(Name::new(string_id, Some(attached_target), None));

            let definition = Definition::SingletonClass(Box::new(SingletonClassDefinition::new(
                name_id,
                self.uri_id,
                offset,
                comments,
                lexical_nesting_id,
            )));

            let definition_id = self.local_graph.add_definition(definition);

            self.add_member_to_current_nesting(definition_id);

            Some(definition_id)
        } else {
            None
        };

        if let Some(body) = node.body() {
            if let Some(definition_id) = definition_id {
                self.definitions_stack.push(definition_id);
                self.visit(&body);
                self.definitions_stack.pop();
            } else {
                self.visit(&body);
            }
        }
    }

    fn visit_constant_and_write_node(&mut self, node: &ruby_prism::ConstantAndWriteNode) {
        self.index_constant_reference(&node.as_node(), true);
        self.visit(&node.value());
    }

    fn visit_constant_operator_write_node(&mut self, node: &ruby_prism::ConstantOperatorWriteNode) {
        self.index_constant_reference(&node.as_node(), true);
        self.visit(&node.value());
    }

    fn visit_constant_or_write_node(&mut self, node: &ruby_prism::ConstantOrWriteNode) {
        self.add_constant_definition(&node.as_node(), true);
        self.visit(&node.value());
    }

    fn visit_constant_write_node(&mut self, node: &ruby_prism::ConstantWriteNode) {
        self.add_constant_definition(&node.as_node(), false);
        self.visit(&node.value());
    }

    fn visit_constant_path_and_write_node(&mut self, node: &ruby_prism::ConstantPathAndWriteNode) {
        self.visit_constant_path_node(&node.target());
    }

    fn visit_constant_path_operator_write_node(&mut self, node: &ruby_prism::ConstantPathOperatorWriteNode) {
        self.visit_constant_path_node(&node.target());
    }

    fn visit_constant_path_or_write_node(&mut self, node: &ruby_prism::ConstantPathOrWriteNode) {
        self.add_constant_definition(&node.target().as_node(), true);
        self.visit(&node.value());
    }

    fn visit_constant_path_write_node(&mut self, node: &ruby_prism::ConstantPathWriteNode) {
        self.add_constant_definition(&node.target().as_node(), false);
        self.visit(&node.value());
    }

    fn visit_constant_read_node(&mut self, node: &ruby_prism::ConstantReadNode<'_>) {
        self.index_constant_reference(&node.as_node(), true);
    }

    fn visit_constant_path_node(&mut self, node: &ruby_prism::ConstantPathNode<'_>) {
        self.index_constant_reference(&node.as_node(), true);
    }

    fn visit_multi_write_node(&mut self, node: &ruby_prism::MultiWriteNode) {
        for left in &node.lefts() {
            match left {
                ruby_prism::Node::ConstantTargetNode { .. } | ruby_prism::Node::ConstantPathTargetNode { .. } => {
                    self.add_constant_definition(&left, false);
                }
                ruby_prism::Node::GlobalVariableTargetNode { .. } => {
                    self.add_definition_from_location(
                        &left.location(),
                        |str_id, offset, comments, lexical_nesting_id, uri_id| {
                            Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                                str_id,
                                uri_id,
                                offset,
                                comments,
                                lexical_nesting_id,
                            )))
                        },
                    );
                }
                ruby_prism::Node::InstanceVariableTargetNode { .. } => {
                    self.add_definition_from_location(
                        &left.location(),
                        |str_id, offset, comments, lexical_nesting_id, uri_id| {
                            Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
                                str_id,
                                uri_id,
                                offset,
                                comments,
                                lexical_nesting_id,
                            )))
                        },
                    );
                }
                ruby_prism::Node::ClassVariableTargetNode { .. } => {
                    self.add_definition_from_location(
                        &left.location(),
                        |str_id, offset, comments, lexical_nesting_id, uri_id| {
                            Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
                                str_id,
                                uri_id,
                                offset,
                                comments,
                                lexical_nesting_id,
                            )))
                        },
                    );
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
        let name = Self::location_to_string(&node.name_loc());
        let str_id = self.local_graph.intern_string(name);
        let offset = Offset::from_prism_location(&node.location());
        let comments = self.find_comments_for(offset.start()).unwrap_or_default();
        let lexical_nesting_id = self.parent_nesting_id().copied();
        let parameters = self.collect_parameters(node);
        let is_singleton = node.receiver().is_some();

        let visibility = if is_singleton {
            Visibility::Public
        } else {
            *self.current_visibility()
        };

        let receiver = if let Some(recv_node) = node.receiver() {
            match recv_node {
                // def self.foo - receiver is the current class/module's NameId
                ruby_prism::Node::SelfNode { .. } => self.current_nesting_name_id(),
                // def Foo.bar or def Foo::Bar.baz - receiver is the constant's NameId
                ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. } => {
                    self.index_constant_reference(&recv_node, true)
                }
                // Dynamic receiver (def foo.bar) - visit and then skip
                // We still want to visit because it could be a variable reference
                _ => {
                    self.visit(&recv_node);
                    return;
                }
            }
        } else {
            None
        };

        let method = Definition::Method(Box::new(MethodDefinition::new(
            str_id,
            self.uri_id,
            offset,
            comments,
            lexical_nesting_id,
            parameters,
            visibility,
            receiver,
        )));

        let definition_id = self.local_graph.add_definition(method);

        self.add_member_to_current_nesting(definition_id);

        if let Some(body) = node.body() {
            self.visit(&body);
        }
    }

    #[allow(clippy::too_many_lines)]
    fn visit_call_node(&mut self, node: &ruby_prism::CallNode) {
        enum AttrKind {
            Accessor,
            Reader,
            Writer,
        }

        let mut index_attr = |kind: AttrKind, call: &ruby_prism::CallNode| {
            Self::each_string_or_symbol_arg(call, |name, location| {
                let str_id = self.local_graph.intern_string(name);
                let lexical_nesting_id = self.parent_nesting_id().copied();
                let offset = Offset::from_prism_location(&location);
                let comments = self.find_comments_for(offset.start()).unwrap_or_default();

                let definition = match kind {
                    AttrKind::Accessor => Definition::AttrAccessor(Box::new(AttrAccessorDefinition::new(
                        str_id,
                        self.uri_id,
                        offset,
                        comments,
                        lexical_nesting_id,
                        *self.current_visibility(),
                    ))),
                    AttrKind::Reader => Definition::AttrReader(Box::new(AttrReaderDefinition::new(
                        str_id,
                        self.uri_id,
                        offset,
                        comments,
                        lexical_nesting_id,
                        *self.current_visibility(),
                    ))),
                    AttrKind::Writer => Definition::AttrWriter(Box::new(AttrWriterDefinition::new(
                        str_id,
                        self.uri_id,
                        offset,
                        comments,
                        lexical_nesting_id,
                        *self.current_visibility(),
                    ))),
                };

                let definition_id = self.local_graph.add_definition(definition);

                self.add_member_to_current_nesting(definition_id);
            });
        };

        let message_loc = node.message_loc();

        if message_loc.is_none() {
            // No message, we can't index this node
            return;
        }

        let message = String::from_utf8_lossy(node.name().as_slice()).to_string();

        match message.as_str() {
            "attr_accessor" => {
                index_attr(AttrKind::Accessor, node);
            }
            "attr_reader" => {
                index_attr(AttrKind::Reader, node);
            }
            "attr_writer" => {
                index_attr(AttrKind::Writer, node);
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
                    index_attr(AttrKind::Accessor, node);
                } else {
                    index_attr(AttrKind::Reader, node);
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
            "include" => {
                let receiver = node.receiver();
                if receiver.is_none() || receiver.as_ref().is_some_and(|r| r.as_self_node().is_some()) {
                    self.handle_mixin(node, MixinType::Include);
                } else {
                    self.visit_call_node_parts(node);
                }
            }
            "prepend" => {
                let receiver = node.receiver();
                if receiver.is_none() || receiver.as_ref().is_some_and(|r| r.as_self_node().is_some()) {
                    self.handle_mixin(node, MixinType::Prepend);
                } else {
                    self.visit_call_node_parts(node);
                }
            }
            "extend" => {
                let receiver = node.receiver();
                if receiver.is_none() || receiver.as_ref().is_some_and(|r| r.as_self_node().is_some()) {
                    self.handle_mixin(node, MixinType::Extend);
                } else {
                    self.visit_call_node_parts(node);
                }
            }
            "private" | "protected" | "public" => {
                if let Some(_receiver) = node.receiver() {
                    self.visit_call_node_parts(node);
                    return;
                }

                self.visibility_stack.push(Visibility::from_string(message.as_str()));

                if let Some(arguments) = node.arguments() {
                    self.visit_arguments_node(&arguments);
                    self.visibility_stack.pop();
                }
            }
            _ => {
                // For method calls that we don't explicitly handle each part, we continue visiting their parts as we
                // may discover something inside
                self.visit_call_node_parts(node);
                self.index_method_reference(message.clone(), &node.message_loc().unwrap());

                match message.as_str() {
                    ">" | "<" | ">=" | "<=" => {
                        self.index_method_reference("<=>".to_string(), &node.message_loc().unwrap());
                    }
                    _ => {}
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
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, lexical_nesting_id, uri_id| {
                Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                    str_id,
                    uri_id,
                    offset,
                    comments,
                    lexical_nesting_id,
                )))
            },
        );
        self.visit(&node.value());
    }

    fn visit_global_variable_and_write_node(&mut self, node: &ruby_prism::GlobalVariableAndWriteNode<'_>) {
        self.add_definition_from_location(&node.name_loc(), |str_id, offset, comments, nesting_id, uri_id| {
            Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                str_id, uri_id, offset, comments, nesting_id,
            )))
        });
        self.visit(&node.value());
    }

    fn visit_global_variable_or_write_node(&mut self, node: &ruby_prism::GlobalVariableOrWriteNode<'_>) {
        self.add_definition_from_location(&node.name_loc(), |str_id, offset, comments, nesting_id, uri_id| {
            Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                str_id, uri_id, offset, comments, nesting_id,
            )))
        });
        self.visit(&node.value());
    }

    fn visit_global_variable_operator_write_node(&mut self, node: &ruby_prism::GlobalVariableOperatorWriteNode<'_>) {
        self.add_definition_from_location(&node.name_loc(), |str_id, offset, comments, nesting_id, uri_id| {
            Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                str_id, uri_id, offset, comments, nesting_id,
            )))
        });
        self.visit(&node.value());
    }

    fn visit_alias_global_variable_node(&mut self, node: &ruby_prism::AliasGlobalVariableNode<'_>) {
        self.add_definition_from_location(
            &node.new_name().location(),
            |str_id, offset, comments, nesting_id, uri_id| {
                Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                    str_id, uri_id, offset, comments, nesting_id,
                )))
            },
        );
    }

    fn visit_instance_variable_and_write_node(&mut self, node: &ruby_prism::InstanceVariableAndWriteNode) {
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, lexical_nesting_id, uri_id| {
                Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
                    str_id,
                    uri_id,
                    offset,
                    comments,
                    lexical_nesting_id,
                )))
            },
        );
        self.visit(&node.value());
    }

    fn visit_instance_variable_operator_write_node(&mut self, node: &ruby_prism::InstanceVariableOperatorWriteNode) {
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, lexical_nesting_id, uri_id| {
                Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
                    str_id,
                    uri_id,
                    offset,
                    comments,
                    lexical_nesting_id,
                )))
            },
        );
        self.visit(&node.value());
    }

    fn visit_instance_variable_or_write_node(&mut self, node: &ruby_prism::InstanceVariableOrWriteNode) {
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, lexical_nesting_id, uri_id| {
                Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
                    str_id,
                    uri_id,
                    offset,
                    comments,
                    lexical_nesting_id,
                )))
            },
        );
        self.visit(&node.value());
    }

    fn visit_instance_variable_write_node(&mut self, node: &ruby_prism::InstanceVariableWriteNode) {
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, lexical_nesting_id, uri_id| {
                Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
                    str_id,
                    uri_id,
                    offset,
                    comments,
                    lexical_nesting_id,
                )))
            },
        );
        self.visit(&node.value());
    }

    fn visit_class_variable_and_write_node(&mut self, node: &ruby_prism::ClassVariableAndWriteNode) {
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, lexical_nesting_id, uri_id| {
                Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
                    str_id,
                    uri_id,
                    offset,
                    comments,
                    lexical_nesting_id,
                )))
            },
        );
        self.visit(&node.value());
    }

    fn visit_class_variable_operator_write_node(&mut self, node: &ruby_prism::ClassVariableOperatorWriteNode) {
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, lexical_nesting_id, uri_id| {
                Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
                    str_id,
                    uri_id,
                    offset,
                    comments,
                    lexical_nesting_id,
                )))
            },
        );
        self.visit(&node.value());
    }

    fn visit_class_variable_or_write_node(&mut self, node: &ruby_prism::ClassVariableOrWriteNode) {
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, lexical_nesting_id, uri_id| {
                Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
                    str_id,
                    uri_id,
                    offset,
                    comments,
                    lexical_nesting_id,
                )))
            },
        );
        self.visit(&node.value());
    }

    fn visit_class_variable_write_node(&mut self, node: &ruby_prism::ClassVariableWriteNode) {
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, lexical_nesting_id, uri_id| {
                Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
                    str_id,
                    uri_id,
                    offset,
                    comments,
                    lexical_nesting_id,
                )))
            },
        );
        self.visit(&node.value());
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
    use crate::{
        model::{
            definitions::{Definition, Mixin, Parameter},
            visibility::Visibility,
        },
        test_utils::LocalGraphTest,
    };

    macro_rules! assert_definition_at {
        ($context:expr, $location:expr, $variant:ident, |$var:ident| $body:block) => {{
            let __def = $context.definition_at($location);
            let __kind = __def.kind();
            match __def {
                Definition::$variant(boxed) => {
                    let $var = &*boxed.as_ref();
                    $body
                }
                _ => panic!("expected {} definition, got {:?}", stringify!($variant), __kind),
            }
        }};
    }

    macro_rules! assert_name_eq {
        ($context:expr, $expect_name_string:expr, $def:expr) => {{
            let actual_name = $context.graph().strings().get($def.str_id()).unwrap();

            assert_eq!($expect_name_string, actual_name);
        }};
    }

    /// Asserts the full path of a definition's `name_id` matches the expected string.
    /// Works with any definition that has a `name_id()` method (`ClassDefinition`, `ModuleDefinition`, `ConstantDefinition`).
    ///
    /// Usage:
    /// - `assert_name_id_to_string_eq!(ctx, "Foo::Bar::Baz", def)` - asserts the full path `Foo::Bar::Baz`
    /// - `assert_name_id_to_string_eq!(ctx, "Baz", def)` - asserts just `Baz` with no parent scope
    macro_rules! assert_name_id_to_string_eq {
        ($context:expr, $expect_path:expr, $def:expr) => {{
            let segments: Vec<&str> = $expect_path.split("::").collect();
            assert!(!segments.is_empty(), "expected path must have at least one segment");

            // The name stores the last segment, with parent_scope chain going backwards
            let expected_name = segments.last().unwrap();
            let expected_parents: Vec<&str> = segments[..segments.len() - 1].iter().rev().copied().collect();

            let name = $context.graph().names().get($def.name_id()).unwrap();
            let actual_name = $context.graph().strings().get(name.str()).unwrap();
            assert_eq!(
                *expected_name, actual_name,
                "constant name mismatch: expected '{}', got '{}'",
                expected_name, actual_name
            );

            let mut current_name = name;
            for (i, expected_parent) in expected_parents.iter().enumerate() {
                if let Some(parent_scope_id) = current_name.parent_scope() {
                    let parent_name = $context.graph().names().get(&parent_scope_id).unwrap();
                    let actual_parent = $context.graph().strings().get(parent_name.str()).unwrap();
                    assert_eq!(
                        *expected_parent, actual_parent,
                        "parent_scope mismatch at depth {}: expected '{}', got '{}'",
                        i, expected_parent, actual_parent
                    );
                    current_name = parent_name;
                } else {
                    panic!(
                        "expected parent_scope '{}' at depth {}, but got None",
                        expected_parent, i
                    );
                }
            }

            assert!(
                current_name.parent_scope().is_none(),
                "expected no more parent_scopes after chain, but got Some"
            );
        }};
    }

    macro_rules! assert_parameter {
        ($expr:expr, $variant:ident, |$param:ident| $body:block) => {
            match $expr {
                Parameter::$variant($param) => $body,
                _ => panic!("expected {} parameter, got {:?}", stringify!($variant), $expr),
            }
        };
    }

    macro_rules! assert_comments_eq {
        ($context:expr, $def:expr, $expected_comments:expr) => {{
            let actual_comments: Vec<String> = $def.comments().iter().map(|c| c.string().to_string()).collect();
            assert_eq!($expected_comments, actual_comments);
        }};
    }

    macro_rules! assert_constant_references_eq {
        ($context:expr, $expected_names:expr) => {{
            let mut actual_references = $context
                .graph()
                .constant_references()
                .values()
                .map(|r| {
                    let name = $context.graph().names().get(r.name_id()).unwrap();
                    (r.offset().start(), $context.graph().strings().get(name.str()).unwrap())
                })
                .collect::<Vec<_>>();

            actual_references.sort();

            let actual_names = actual_references
                .iter()
                .map(|(_, name)| name.as_str())
                .collect::<Vec<_>>();

            assert_eq!($expected_names, actual_names);
        }};
    }

    macro_rules! assert_method_references_eq {
        ($context:expr, $expected_names:expr) => {{
            let mut actual_references = $context
                .graph()
                .method_references()
                .values()
                .map(|m| (m.offset().start(), $context.graph().strings().get(m.str()).unwrap()))
                .collect::<Vec<_>>();

            actual_references.sort();

            let actual_names = actual_references
                .iter()
                .map(|(_offset, name)| name.as_str())
                .collect::<Vec<_>>();

            assert_eq!($expected_names, actual_names);
        }};
    }

    macro_rules! assert_method_has_receiver {
        ($context:expr, $method:expr, $expected_receiver:expr) => {{
            if let Some(receiver_name_id) = $method.receiver() {
                let name = $context.graph().names().get(receiver_name_id).unwrap();
                let actual_name = $context.graph().strings().get(name.str()).unwrap();
                assert_eq!($expected_receiver, actual_name);
            } else {
                panic!("expected method to have receiver, got None");
            }
        }};
    }

    macro_rules! assert_includes_eq {
        ($context:expr, $def:expr, $expected_names:expr) => {{
            let actual_names = $def
                .mixins()
                .iter()
                .filter_map(|mixin| {
                    if let Mixin::Include(name_id) = mixin {
                        let name = $context.graph().names().get(name_id).unwrap();
                        Some($context.graph().strings().get(name.str()).unwrap().as_str())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            assert_eq!($expected_names, actual_names);
        }};
    }

    macro_rules! assert_prepends_eq {
        ($context:expr, $def:expr, $expected_names:expr) => {{
            let actual_names = $def
                .mixins()
                .iter()
                .filter_map(|mixin| {
                    if let Mixin::Prepend(name_id) = mixin {
                        let name = $context.graph().names().get(name_id).unwrap();
                        Some($context.graph().strings().get(name.str()).unwrap().as_str())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            assert_eq!($expected_names, actual_names);
        }};
    }

    macro_rules! assert_extends_eq {
        ($context:expr, $def:expr, $expected_names:expr) => {{
            let actual_names = $def
                .mixins()
                .iter()
                .filter_map(|mixin| {
                    if let Mixin::Extend(name_id) = mixin {
                        let name = $context.graph().names().get(&name_id).unwrap();
                        Some($context.graph().strings().get(name.str()).unwrap().as_str())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            assert_eq!($expected_names, actual_names);
        }};
    }

    macro_rules! assert_string_eq {
        ($context:expr, $str_id:expr, $expected_name:expr) => {{
            let string_name = $context.graph().strings().get($str_id).unwrap();
            assert_eq!(string_name, $expected_name);
        }};
    }

    macro_rules! assert_diagnostics_eq {
        ($context:expr, $expected_diagnostics:expr) => {{
            let actual_diagnostics: Vec<String> = $context
                .graph()
                .diagnostics()
                .iter()
                .map(|d| {
                    format!(
                        "{}: {} ({})",
                        d.severity().as_str(),
                        d.message(),
                        $context.offset_to_display_range(d.offset())
                    )
                })
                .collect();
            assert_eq!($expected_diagnostics, actual_diagnostics);
        }};
    }

    macro_rules! assert_no_diagnostics {
        ($context:expr) => {{
            assert!(
                $context.graph().diagnostics().is_empty(),
                "expected no diagnostics, got {:?}",
                $context
                    .graph()
                    .diagnostics()
                    .iter()
                    .map(|d| format!(
                        "{}: {} ({})",
                        d.severity().as_str(),
                        d.message(),
                        $context.offset_to_display_range(d.offset())
                    ))
                    .collect::<Vec<_>>()
            );
        }};
    }

    fn index_source(source: &str) -> LocalGraphTest {
        LocalGraphTest::new("file:///foo.rb", source)
    }

    #[test]
    fn index_source_with_errors() {
        let context = index_source({
            "
            class Foo
            "
        });

        assert_diagnostics_eq!(
            &context,
            vec![
                "Error: unexpected end-of-input, assuming it is closing the parent top level context (1:10-2:1)",
                "Error: expected an `end` to close the `class` statement (2:1-2:1)"
            ]
        );

        // We still index the definition, even though it has errors
        assert_eq!(context.graph().definitions().len(), 1);
        assert_definition_at!(&context, "1:1-2:1", Class, |def| {
            assert_name_id_to_string_eq!(&context, "Foo", def);
        });
    }

    #[test]
    fn index_source_with_warnings() {
        let context = index_source({
            "
            foo = 42
            "
        });

        assert_diagnostics_eq!(&context, vec!["Warning: assigned but unused variable - foo (1:1-1:4)"]);
    }

    #[test]
    fn index_class_node() {
        let context = index_source({
            "
            class Foo
              class Bar
                class Baz; end
              end
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 3);

        assert_definition_at!(&context, "1:1-5:4", Class, |def| {
            assert_name_id_to_string_eq!(&context, "Foo", def);
            assert!(def.superclass_ref().is_none());
            assert_eq!(1, def.members().len());
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:3-4:6", Class, |def| {
            assert_name_id_to_string_eq!(&context, "Bar", def);
            assert!(def.superclass_ref().is_none());
            assert_eq!(1, def.members().len());

            assert_definition_at!(&context, "1:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "3:5-3:19", Class, |def| {
            assert_name_id_to_string_eq!(&context, "Baz", def);
            assert!(def.superclass_ref().is_none());
            assert!(def.members().is_empty());

            assert_definition_at!(&context, "2:3-4:6", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });
    }

    #[test]
    fn index_class_node_with_qualified_name() {
        let context = index_source({
            "
            class Foo::Bar
              class Baz::Qux
                class ::Quuux; end
              end
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 3);

        assert_definition_at!(&context, "1:1-5:4", Class, |def| {
            assert_name_id_to_string_eq!(&context, "Foo::Bar", def);
            assert!(def.superclass_ref().is_none());
            assert!(def.lexical_nesting_id().is_none());
            assert_eq!(1, def.members().len());
        });

        assert_definition_at!(&context, "2:3-4:6", Class, |def| {
            assert_name_id_to_string_eq!(&context, "Baz::Qux", def);
            assert!(def.superclass_ref().is_none());
            assert_eq!(1, def.members().len());

            assert_definition_at!(&context, "1:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "3:5-3:23", Class, |def| {
            assert_name_id_to_string_eq!(&context, "Quuux", def);
            assert!(def.superclass_ref().is_none());
            assert!(def.members().is_empty());

            assert_definition_at!(&context, "2:3-4:6", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });
    }

    #[test]
    fn index_class_with_dynamic_names() {
        let context = index_source({
            "
            class foo::Bar
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert!(context.graph().definitions().is_empty());
    }

    #[test]
    fn index_module_node() {
        let context = index_source({
            "
            module Foo
              module Bar
                module Baz; end
              end
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 3);

        assert_definition_at!(&context, "1:1-5:4", Module, |def| {
            assert_name_id_to_string_eq!(&context, "Foo", def);
            assert_eq!(1, def.members().len());
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:3-4:6", Module, |def| {
            assert_name_id_to_string_eq!(&context, "Bar", def);
            assert_eq!(1, def.members().len());

            assert_definition_at!(&context, "1:1-5:4", Module, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "3:5-3:20", Module, |def| {
            assert_name_id_to_string_eq!(&context, "Baz", def);

            assert_definition_at!(&context, "2:3-4:6", Module, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });
    }

    #[test]
    fn index_module_node_with_qualified_name() {
        let context = index_source({
            "
            module Foo::Bar
              module Baz::Qux
                module ::Quuux; end
              end
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 3);

        assert_definition_at!(&context, "1:1-5:4", Module, |def| {
            assert_name_id_to_string_eq!(&context, "Foo::Bar", def);
            assert_eq!(1, def.members().len());
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:3-4:6", Module, |def| {
            assert_name_id_to_string_eq!(&context, "Baz::Qux", def);
            assert_eq!(1, def.members().len());

            assert_definition_at!(&context, "1:1-5:4", Module, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "3:5-3:24", Module, |def| {
            assert_name_id_to_string_eq!(&context, "Quuux", def);

            assert_definition_at!(&context, "2:3-4:6", Module, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });
    }

    #[test]
    fn index_module_with_dynamic_names() {
        let context = index_source({
            "
            module foo::Bar
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert!(context.graph().definitions().is_empty());
    }

    #[test]
    fn index_constant_write_node() {
        let context = index_source({
            "
            FOO = 1

            class Foo
              FOO = 2
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 3);

        assert_definition_at!(&context, "1:1-1:4", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "FOO", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:3-4:6", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "FOO", def);

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });
    }

    #[test]
    fn index_constant_path_write_node() {
        let context = index_source({
            "
            FOO::BAR = 1

            class Foo
              FOO::BAR = 2
              ::BAZ = 3
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 4);

        assert_definition_at!(&context, "1:1-1:9", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "FOO::BAR", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:3-4:11", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "FOO::BAR", def);

            assert_definition_at!(&context, "3:1-6:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "5:3-5:8", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "BAZ", def);

            assert_definition_at!(&context, "3:1-6:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[1], def.id());
            });
        });
    }

    #[test]
    fn index_constant_or_write_node() {
        let context = index_source({
            "
            FOO ||= 1

            class Bar
              BAZ ||= 2
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 3);

        assert_definition_at!(&context, "1:1-1:4", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "FOO", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:3-4:6", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "BAZ", def);

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_constant_references_eq!(&context, vec!["FOO", "BAZ"]);
    }

    #[test]
    fn index_constant_path_or_write_node() {
        let context = index_source({
            "
            FOO::BAR ||= 1

            class MyClass
              FOO::BAR ||= 2
              ::BAZ ||= 3
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 4);

        assert_definition_at!(&context, "1:1-1:9", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "FOO::BAR", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:3-4:11", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "FOO::BAR", def);

            assert_definition_at!(&context, "3:1-6:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "5:3-5:8", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "BAZ", def);

            assert_definition_at!(&context, "3:1-6:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[1], def.id());
            });
        });

        assert_constant_references_eq!(&context, vec!["FOO", "BAR", "FOO", "BAR", "BAZ"]);
    }

    #[test]
    fn index_constant_multi_write_node() {
        let context = index_source({
            "
            FOO, BAR::BAZ = 1, 2

            class Foo
              FOO, BAR::BAZ, ::BAZ = 3, 4, 5
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 6);

        assert_definition_at!(&context, "1:1-1:4", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "FOO", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "1:6-1:14", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "BAR::BAZ", def);
        });

        assert_definition_at!(&context, "4:3-4:6", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "FOO", def);

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "4:8-4:16", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "BAR::BAZ", def);

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[1], def.id());
            });
        });

        assert_definition_at!(&context, "4:18-4:23", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "BAZ", def);

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[2], def.id());
            });
        });
    }

    #[test]
    fn index_def_node() {
        let context = index_source({
            "
            def foo; end

            class Foo
              def bar; end
              def self.baz; end
            end

            class Bar
              def Foo.quz; end
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 6);

        assert_definition_at!(&context, "1:1-1:13", Method, |def| {
            assert_name_eq!(&context, "foo", def);
            assert_eq!(def.parameters().len(), 0);
            assert!(def.receiver().is_none());
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "3:1-6:4", Class, |foo_class_def| {
            assert_definition_at!(&context, "4:3-4:15", Method, |bar_def| {
                assert_name_eq!(&context, "bar", bar_def);
                assert_eq!(bar_def.parameters().len(), 0);
                assert!(bar_def.receiver().is_none());
                assert_eq!(foo_class_def.id(), bar_def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[0], bar_def.id());
            });

            assert_definition_at!(&context, "5:3-5:20", Method, |baz_def| {
                assert_name_eq!(&context, "baz", baz_def);
                assert_eq!(baz_def.parameters().len(), 0);
                assert_method_has_receiver!(&context, baz_def, "Foo");
                assert_eq!(foo_class_def.id(), baz_def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[1], baz_def.id());
            });
        });

        assert_definition_at!(&context, "8:1-10:4", Class, |bar_class_def| {
            assert_name_id_to_string_eq!(&context, "Bar", bar_class_def);

            assert_definition_at!(&context, "9:3-9:19", Method, |quz_def| {
                assert_name_eq!(&context, "quz", quz_def);
                assert_eq!(quz_def.parameters().len(), 0);
                assert_method_has_receiver!(&context, quz_def, "Foo");
                assert_eq!(bar_class_def.id(), quz_def.lexical_nesting_id().unwrap());
            });
        });
    }

    #[test]
    fn do_not_index_def_node_with_dynamic_receiver() {
        let context = index_source({
            "
            def foo.bar; end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 0);
        assert_method_references_eq!(&context, vec!["foo"]);
    }

    #[test]
    fn index_class_self_block_creates_singleton_class() {
        let context = index_source({
            "
            class Bar; end

            class Foo
              class << self
                def baz; end

                class << Bar
                  def self.qux; end
                end

                class << self
                  def quz; end
                end
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        // class Bar
        assert_definition_at!(&context, "1:1-1:15", Class, |bar_class| {
            assert_name_id_to_string_eq!(&context, "Bar", bar_class);
        });

        // class Foo
        assert_definition_at!(&context, "3:1-15:4", Class, |foo_class| {
            assert_name_id_to_string_eq!(&context, "Foo", foo_class);

            // class << self (inside Foo)
            assert_definition_at!(&context, "4:3-14:6", SingletonClass, |foo_singleton| {
                assert_name_id_to_string_eq!(&context, "Foo::<Foo>", foo_singleton);
                assert_eq!(foo_singleton.lexical_nesting_id(), &Some(foo_class.id()));

                // def baz (inside class << self)
                assert_definition_at!(&context, "5:5-5:17", Method, |baz_method| {
                    assert_eq!(baz_method.lexical_nesting_id(), &Some(foo_singleton.id()));
                });

                // class << Bar (inside class << self of Foo)
                assert_definition_at!(&context, "7:5-9:8", SingletonClass, |bar_singleton| {
                    assert_name_id_to_string_eq!(&context, "Bar::<Bar>", bar_singleton);
                    assert_eq!(bar_singleton.lexical_nesting_id(), &Some(foo_singleton.id()));

                    // def self.qux (inside class << Bar)
                    assert_definition_at!(&context, "8:7-8:24", Method, |qux_method| {
                        assert_eq!(qux_method.lexical_nesting_id(), &Some(bar_singleton.id()));
                        assert_method_has_receiver!(&context, qux_method, "<Bar>");
                    });
                });

                // class << self (nested inside outer class << self)
                assert_definition_at!(&context, "11:5-13:8", SingletonClass, |nested_singleton| {
                    assert_name_id_to_string_eq!(&context, "Foo::<Foo>::<<Foo>>", nested_singleton);
                    assert_eq!(nested_singleton.lexical_nesting_id(), &Some(foo_singleton.id()));

                    // def quz (inside nested class << self)
                    assert_definition_at!(&context, "12:7-12:19", Method, |quz_method| {
                        assert_eq!(quz_method.lexical_nesting_id(), &Some(nested_singleton.id()));
                    });
                });
            });
        });
    }

    #[test]
    fn index_singleton_class_definition_in_compact_namespace() {
        let context = index_source({
            "
            class Foo::Bar
              class << self
                def baz; end
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-5:4", Class, |class_def| {
            assert_name_id_to_string_eq!(&context, "Foo::Bar", class_def);
            assert_definition_at!(&context, "2:3-4:6", SingletonClass, |singleton_class| {
                assert_eq!(singleton_class.lexical_nesting_id(), &Some(class_def.id()));
                assert_definition_at!(&context, "3:5-3:17", Method, |method| {
                    assert_eq!(method.lexical_nesting_id(), &Some(singleton_class.id()));
                });
            });
        });

        assert_constant_references_eq!(&context, vec!["Foo"]);
    }

    #[test]
    fn index_constant_in_singleton_class_definition() {
        let context = index_source({
            "
            class Foo
              class << self
                A = 1
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-5:4", Class, |class_def| {
            assert_definition_at!(&context, "2:3-4:6", SingletonClass, |singleton_class| {
                assert_eq!(singleton_class.lexical_nesting_id(), &Some(class_def.id()));
                assert_definition_at!(&context, "3:5-3:6", Constant, |def| {
                    assert_name_id_to_string_eq!(&context, "A", def);
                    assert_eq!(Some(singleton_class.id()), def.lexical_nesting_id().clone());
                });
            });
        });
    }

    #[test]
    fn do_not_index_singleton_class_with_dynamic_expression() {
        let context = index_source({
            "
            class << foo
              def bar; end
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 0);
    }

    #[test]
    fn index_class_variable_in_singleton_class_definition() {
        let context = index_source({
            "
            class Foo
              class << self
                @@var = 1
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        // During indexing, lexical_nesting_id is the actual enclosing scope (singleton class).
        // The resolution phase handles bypassing singleton classes for class variable ownership.
        assert_definition_at!(&context, "2:3-4:6", SingletonClass, |singleton_class| {
            assert_definition_at!(&context, "3:5-3:10", ClassVariable, |def| {
                assert_name_eq!(&context, "@@var", def);
                assert_eq!(Some(singleton_class.id()), *def.lexical_nesting_id());
            });
        });
    }

    #[test]
    fn index_class_variable_in_nested_singleton_class_definition() {
        let context = index_source({
            "
            class Foo
              class << self
                class << self
                  @@var = 1
                end
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        // During indexing, lexical_nesting_id is the actual enclosing scope (innermost singleton class).
        // The resolution phase handles bypassing singleton classes for class variable ownership.
        assert_definition_at!(&context, "3:5-5:8", SingletonClass, |nested_singleton| {
            assert_definition_at!(&context, "4:7-4:12", ClassVariable, |def| {
                assert_name_eq!(&context, "@@var", def);
                assert_eq!(Some(nested_singleton.id()), *def.lexical_nesting_id());
            });
        });
    }

    #[test]
    fn index_class_variable_in_singleton_method_definition() {
        let context = index_source({
            "
            class Foo
              def self.bar
                @@var = 1
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-5:4", Class, |class_def| {
            assert_definition_at!(&context, "3:5-3:10", ClassVariable, |def| {
                assert_name_eq!(&context, "@@var", def);
                assert_eq!(Some(class_def.id()), def.lexical_nesting_id().clone());
            });
        });
    }

    #[test]
    fn index_def_node_with_parameters() {
        let context = index_source({
            "
            def foo(a, b = 42, *c, d, e:, g: 42, **i, &j); end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:51", Method, |def| {
            assert_eq!(def.parameters().len(), 8);

            assert_parameter!(&def.parameters()[0], RequiredPositional, |param| {
                assert_string_eq!(context, param.str(), "a");
            });

            assert_parameter!(&def.parameters()[1], OptionalPositional, |param| {
                assert_string_eq!(context, param.str(), "b");
            });

            assert_parameter!(&def.parameters()[2], RestPositional, |param| {
                assert_string_eq!(context, param.str(), "c");
            });

            assert_parameter!(&def.parameters()[3], Post, |param| {
                assert_string_eq!(context, param.str(), "d");
            });

            assert_parameter!(&def.parameters()[4], RequiredKeyword, |param| {
                assert_string_eq!(context, param.str(), "e");
            });

            assert_parameter!(&def.parameters()[5], OptionalKeyword, |param| {
                assert_string_eq!(context, param.str(), "g");
            });

            assert_parameter!(&def.parameters()[6], RestKeyword, |param| {
                assert_string_eq!(context, param.str(), "i");
            });

            assert_parameter!(&def.parameters()[7], Block, |param| {
                assert_string_eq!(context, param.str(), "j");
            });
        });
    }

    #[test]
    fn index_def_node_with_forward_parameters() {
        let context = index_source({
            "
            def foo(...); end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:18", Method, |def| {
            assert_eq!(def.parameters().len(), 1);
            assert_parameter!(&def.parameters()[0], Forward, |param| {
                assert_string_eq!(context, param.str(), "...");
            });
        });
    }

    #[test]
    fn index_def_node_with_visibility_top_level() {
        let context = index_source({
            "
            def m1; end

            protected def m2; end

            public

            def m3; end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:12", Method, |def| {
            assert_name_eq!(&context, "m1", def);
            assert_eq!(def.visibility(), &Visibility::Private);
        });

        assert_definition_at!(&context, "3:11-3:22", Method, |def| {
            assert_name_eq!(&context, "m2", def);
            assert_eq!(def.visibility(), &Visibility::Protected);
        });

        assert_definition_at!(&context, "7:1-7:12", Method, |def| {
            assert_name_eq!(&context, "m3", def);
            assert_eq!(def.visibility(), &Visibility::Public);
        });
    }

    #[test]
    fn index_def_node_with_visibility_nested() {
        let context = index_source({
            "
            protected

            class Foo
              def m1; end

              private

              module Bar
                def m2; end

                private

                def m3; end

                protected
              end

              def m4; end
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "4:3-4:14", Method, |def| {
            assert_name_eq!(&context, "m1", def);
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "9:5-9:16", Method, |def| {
            assert_name_eq!(&context, "m2", def);
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "13:5-13:16", Method, |def| {
            assert_name_eq!(&context, "m3", def);
            assert_eq!(def.visibility(), &Visibility::Private);
        });

        assert_definition_at!(&context, "18:3-18:14", Method, |def| {
            assert_name_eq!(&context, "m4", def);
            assert_eq!(def.visibility(), &Visibility::Private);
        });
    }

    #[test]
    fn index_def_node_singleton_visibility() {
        let context = index_source({
            "
            protected

            def self.m1; end

            protected def self.m2; end

            class Foo
              private

              def self.m3; end
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "3:1-3:17", Method, |def| {
            assert_name_eq!(&context, "m1", def);
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "5:11-5:27", Method, |def| {
            assert_name_eq!(&context, "m2", def);
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "10:3-10:19", Method, |def| {
            assert_name_eq!(&context, "m3", def);
            assert_eq!(def.visibility(), &Visibility::Public);
        });
    }

    #[test]
    fn index_attr_accessor_definition() {
        let context = index_source({
            "
            attr_accessor :foo

            class Foo
              attr_accessor :bar, :baz
            end

            foo.attr_accessor :not_indexed
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 4);

        assert_definition_at!(&context, "1:16-1:19", AttrAccessor, |def| {
            assert_name_eq!(&context, "foo", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:18-4:21", AttrAccessor, |def| {
            assert_name_eq!(&context, "bar", def);

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "4:24-4:27", AttrAccessor, |def| {
            assert_name_eq!(&context, "baz", def);

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[1], def.id());
            });
        });
    }

    #[test]
    fn index_attr_reader_definition() {
        let context = index_source({
            "
            attr_reader :foo

            class Foo
              attr_reader :bar, :baz
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 4);

        assert_definition_at!(&context, "1:14-1:17", AttrReader, |def| {
            assert_name_eq!(&context, "foo", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:16-4:19", AttrReader, |def| {
            assert_name_eq!(&context, "bar", def);

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "4:22-4:25", AttrReader, |def| {
            assert_name_eq!(&context, "baz", def);

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[1], def.id());
            });
        });
    }

    #[test]
    fn index_attr_writer_definition() {
        let context = index_source({
            "
            attr_writer :foo

            class Foo
              attr_writer :bar, :baz
            end
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 4);

        assert_definition_at!(&context, "1:14-1:17", AttrWriter, |def| {
            assert_name_eq!(&context, "foo", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:16-4:19", AttrWriter, |def| {
            assert_name_eq!(&context, "bar", def);

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "4:22-4:25", AttrWriter, |def| {
            assert_name_eq!(&context, "baz", def);

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[1], def.id());
            });
        });
    }

    #[test]
    fn index_attr_definition() {
        let context = index_source({
            r#"
            attr "a1", :a2

            class Foo
              attr "a3", true
              attr :a4, false
              attr :a5, 123
            end
            "#
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 6);

        assert_definition_at!(&context, "1:6-1:10", AttrReader, |def| {
            assert_name_eq!(&context, "a1", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "1:13-1:15", AttrReader, |def| {
            assert_name_eq!(&context, "a2", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:8-4:12", AttrAccessor, |def| {
            assert_name_eq!(&context, "a3", def);

            assert_definition_at!(&context, "3:1-7:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "5:9-5:11", AttrReader, |def| {
            assert_name_eq!(&context, "a4", def);

            assert_definition_at!(&context, "3:1-7:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[1], def.id());
            });
        });

        assert_definition_at!(&context, "6:9-6:11", AttrReader, |def| {
            assert_name_eq!(&context, "a5", def);

            assert_definition_at!(&context, "3:1-7:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[2], def.id());
            });
        });
    }

    #[test]
    fn index_attr_accessor_with_visibility_top_level() {
        let context = index_source({
            "
            attr_accessor :foo

            protected attr_reader :bar

            public

            attr_writer :baz
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:16-1:19", AttrAccessor, |def| {
            assert_name_eq!(&context, "foo", def);
            assert_eq!(def.visibility(), &Visibility::Private);
        });

        assert_definition_at!(&context, "3:24-3:27", AttrReader, |def| {
            assert_name_eq!(&context, "bar", def);
            assert_eq!(def.visibility(), &Visibility::Protected);
        });

        assert_definition_at!(&context, "7:14-7:17", AttrWriter, |def| {
            assert_name_eq!(&context, "baz", def);
            assert_eq!(def.visibility(), &Visibility::Public);
        });
    }

    #[test]
    fn index_attr_accessor_with_visibility_nested() {
        let context = index_source({
            "
            protected

            class Foo
              attr_accessor :foo

              private

              module Bar
                attr_accessor :bar

                private

                attr_reader :baz

                protected
              end

              attr_writer :qux
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "4:18-4:21", AttrAccessor, |def| {
            assert_name_eq!(&context, "foo", def);
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "9:20-9:23", AttrAccessor, |def| {
            assert_name_eq!(&context, "bar", def);
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "13:18-13:21", AttrReader, |def| {
            assert_name_eq!(&context, "baz", def);
            assert_eq!(def.visibility(), &Visibility::Private);
        });

        assert_definition_at!(&context, "18:16-18:19", AttrWriter, |def| {
            assert_name_eq!(&context, "qux", def);
            assert_eq!(def.visibility(), &Visibility::Private);
        });
    }

    #[test]
    fn index_global_variable_definition() {
        let context = index_source({
            "
            $foo = 1
            $bar, $baz = 2, 3

            class Foo
              $qux = 2
            end

            $one &= 1
            $two &&= 1
            $three ||= 1
            alias $new $one
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 9);

        assert_definition_at!(&context, "1:1-1:5", GlobalVariable, |def| {
            assert_name_eq!(&context, "$foo", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:1-2:5", GlobalVariable, |def| {
            assert_name_eq!(&context, "$bar", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:7-2:11", GlobalVariable, |def| {
            assert_name_eq!(&context, "$baz", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "5:3-5:7", GlobalVariable, |def| {
            assert_name_eq!(&context, "$qux", def);

            assert_definition_at!(&context, "4:1-6:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "8:1-8:5", GlobalVariable, |def| {
            assert_name_eq!(&context, "$one", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "9:1-9:5", GlobalVariable, |def| {
            assert_name_eq!(&context, "$two", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "10:1-10:7", GlobalVariable, |def| {
            assert_name_eq!(&context, "$three", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "11:7-11:11", GlobalVariable, |def| {
            assert_name_eq!(&context, "$new", def);
            assert!(def.lexical_nesting_id().is_none());
        });
    }

    #[test]
    fn index_instance_variable_definition() {
        let context = index_source({
            "
            @foo = 1

            class Foo
              @bar = 2
              @baz, @qux = 3, 4
            end

            @bar &= 5
            @baz &&= 6
            @qux ||= 7

            class Bar
              @foo &= 8
              @bar &&= 9
              @baz ||= 10
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:5", InstanceVariable, |def| {
            assert_name_eq!(&context, "@foo", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "3:1-6:4", Class, |foo_class_def| {
            assert_definition_at!(&context, "4:3-4:7", InstanceVariable, |def| {
                assert_name_eq!(&context, "@bar", def);
                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[0], def.id());
            });

            assert_definition_at!(&context, "5:3-5:7", InstanceVariable, |def| {
                assert_name_eq!(&context, "@baz", def);
                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[1], def.id());
            });

            assert_definition_at!(&context, "5:9-5:13", InstanceVariable, |def| {
                assert_name_eq!(&context, "@qux", def);
                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[2], def.id());
            });
        });

        assert_definition_at!(&context, "8:1-8:5", InstanceVariable, |def| {
            assert_name_eq!(&context, "@bar", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "9:1-9:5", InstanceVariable, |def| {
            assert_name_eq!(&context, "@baz", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "10:1-10:5", InstanceVariable, |def| {
            assert_name_eq!(&context, "@qux", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "12:1-16:4", Class, |bar_class_def| {
            assert_definition_at!(&context, "13:3-13:7", InstanceVariable, |def| {
                assert_name_eq!(&context, "@foo", def);
                assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(bar_class_def.members()[0], def.id());
            });

            assert_definition_at!(&context, "14:3-14:7", InstanceVariable, |def| {
                assert_name_eq!(&context, "@bar", def);
                assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(bar_class_def.members()[1], def.id());
            });

            assert_definition_at!(&context, "15:3-15:7", InstanceVariable, |def| {
                assert_name_eq!(&context, "@baz", def);
                assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(bar_class_def.members()[2], def.id());
            });
        });
    }

    #[test]
    fn index_class_variable_definition() {
        let context = index_source({
            "
            @@foo = 1

            class Foo
              @@bar = 2
              @@baz, @@qux = 3, 4
            end

            @@bar &= 5
            @@baz &&= 6
            @@qux ||= 7

            class Bar
              @@foo &= 1
              @@bar &&= 2
              @@baz ||= 3
            end
            "
        });

        // This is actually not allowed in Ruby and will raise a runtime error
        // But we should still index it so we can insert a diagnostic for it
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:6", ClassVariable, |def| {
            assert_name_eq!(&context, "@@foo", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "3:1-6:4", Class, |foo_class_def| {
            assert_definition_at!(&context, "4:3-4:8", ClassVariable, |def| {
                assert_name_eq!(&context, "@@bar", def);
                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[0], def.id());
            });

            assert_definition_at!(&context, "5:3-5:8", ClassVariable, |def| {
                assert_name_eq!(&context, "@@baz", def);
                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[1], def.id());
            });

            assert_definition_at!(&context, "5:10-5:15", ClassVariable, |def| {
                assert_name_eq!(&context, "@@qux", def);
                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[2], def.id());
            });
        });

        assert_definition_at!(&context, "8:1-8:6", ClassVariable, |def| {
            assert_name_eq!(&context, "@@bar", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "9:1-9:6", ClassVariable, |def| {
            assert_name_eq!(&context, "@@baz", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "10:1-10:6", ClassVariable, |def| {
            assert_name_eq!(&context, "@@qux", def);
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "12:1-16:4", Class, |bar_class_def| {
            assert_definition_at!(&context, "13:3-13:8", ClassVariable, |def| {
                assert_name_eq!(&context, "@@foo", def);
                assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(bar_class_def.members()[0], def.id());
            });

            assert_definition_at!(&context, "14:3-14:8", ClassVariable, |def| {
                assert_name_eq!(&context, "@@bar", def);
                assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(bar_class_def.members()[1], def.id());
            });

            assert_definition_at!(&context, "15:3-15:8", ClassVariable, |def| {
                assert_name_eq!(&context, "@@baz", def);
                assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(bar_class_def.members()[2], def.id());
            });
        });
    }

    #[test]
    fn index_comments_attached_to_definitions() {
        let context = index_source({
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

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "2:1-2:18", Class, |def| {
            assert_name_id_to_string_eq!(&context, "Single", def);
            assert_comments_eq!(&context, def, vec!["# Single comment"]);
        });

        assert_definition_at!(&context, "7:1-7:18", Module, |def| {
            assert_name_id_to_string_eq!(&context, "Multi", def);
            assert_comments_eq!(
                &context,
                def,
                vec![
                    "# Multi-line comment 1",
                    "# Multi-line comment 2",
                    "# Multi-line comment 3"
                ]
            );
        });

        assert_definition_at!(&context, "12:1-12:28", Class, |def| {
            assert_name_id_to_string_eq!(&context, "EmptyCommentLine", def);
            assert_comments_eq!(&context, def, vec!["# Comment 1", "#", "# Comment 2"]);
        });

        assert_definition_at!(&context, "15:1-15:6", Constant, |def| {
            assert_name_id_to_string_eq!(&context, "NoGap", def);
            assert_comments_eq!(&context, def, vec!["# Comment directly above (no gap)"]);
        });

        assert_definition_at!(&context, "19:1-19:13", Method, |def| {
            assert_name_eq!(&context, "foo", def);
            assert_comments_eq!(&context, def, vec!["#: ()", "#| -> void"]);
        });

        assert_definition_at!(&context, "23:1-23:21", Class, |def| {
            assert_name_id_to_string_eq!(&context, "BlankLine", def);
            assert_comments_eq!(&context, def, vec!["# Comment with blank line"]);
        });

        assert_definition_at!(&context, "28:1-28:21", Class, |def| {
            assert_name_id_to_string_eq!(&context, "NoComment", def);
            assert!(def.comments().is_empty());
        });
    }

    #[test]
    fn index_comments_indented_and_nested() {
        let context = index_source({
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

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "2:1-12:4", Class, |def| {
            assert_name_id_to_string_eq!(&context, "Outer", def);
            assert_comments_eq!(&context, def, vec!["# Outer class"]);
        });

        assert_definition_at!(&context, "4:3-7:6", Class, |def| {
            assert_name_id_to_string_eq!(&context, "Inner", def);
            assert_comments_eq!(&context, def, vec!["# Inner class at 2 spaces"]);
        });

        assert_definition_at!(&context, "6:5-6:20", Class, |def| {
            assert_name_id_to_string_eq!(&context, "Deep", def);
            assert_comments_eq!(&context, def, vec!["# Deep class at 4 spaces"]);
        });

        assert_definition_at!(&context, "11:3-11:26", Class, |def| {
            assert_name_id_to_string_eq!(&context, "AnotherInner", def);
            assert_comments_eq!(&context, def, vec!["# Another inner class", "# with multiple lines"]);
        });
    }

    #[test]
    fn index_unresolved_constant_references() {
        let context = index_source({
            "
            puts C1
            puts C2::C3::C4
            puts foo::IGNORED0
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

        assert_diagnostics_eq!(&context, vec!["Warning: assigned but unused variable - foo (5:1-5:4)"]);

        assert_constant_references_eq!(
            &context,
            vec![
                "C1", "C2", "C3", "C4", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17",
                "C18", "C19", "C20", "C21", "C22", "C23"
            ]
        );
    }

    #[test]
    fn index_unresolved_constant_references_from_values() {
        let context = index_source({
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

        assert_no_diagnostics!(&context);

        assert_constant_references_eq!(
            &context,
            vec![
                "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14"
            ]
        );
    }

    #[test]
    fn index_unresolved_constant_references_for_classes() {
        let context = index_source({
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

        assert_no_diagnostics!(&context);

        assert_constant_references_eq!(
            &context,
            vec!["C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11"]
        );
    }

    #[test]
    fn index_unresolved_constant_references_for_modules() {
        let context = index_source({
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

        assert_no_diagnostics!(&context);

        assert_constant_references_eq!(
            &context,
            vec![
                "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12", "M13", "M14", "M15", "M16",
                "M17", "M18", "M19",
            ]
        );
    }

    #[test]
    fn index_method_reference_references() {
        let context = index_source({
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

        assert_diagnostics_eq!(
            &context,
            vec!["Error: unexpected ... when the parent method is not forwarding (31:5-31:8)"]
        );

        assert_method_references_eq!(
            &context,
            vec![
                "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16",
                "m17", "m18", "m19", "m20", "m21", "m22", "m23", "m24", "m25", "m26", "!", "m27", "m28", "m29", "m30",
                "m31", "m32", "m33", "m34", "m35", "m36", "[]", "m37", "m38", "m39", "m40", "m41", "m42", "m43", "m44",
                "m45", "m46", "m47", "m48", "m49", "m50"
            ]
        );
    }

    #[test]
    fn index_method_reference_assign_references() {
        let context = index_source({
            "
            self.m1 = m2
            m3.m4.m5 = m6.m7.m8
            self.m9, self.m10 = m11, m12
            "
        });

        assert_no_diagnostics!(&context);

        assert_method_references_eq!(
            &context,
            vec![
                "m1=", "m2", "m3", "m4", "m5=", "m6", "m7", "m8", "m9=", "m10=", "m11", "m12"
            ]
        );
    }

    #[test]
    fn index_method_reference_opassign_references() {
        let context = index_source({
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

        assert_no_diagnostics!(&context);

        assert_method_references_eq!(
            &context,
            vec![
                "m1", "m1=", "m2", "m2=", "m3", "m3=", "m4", "m4=", "m5", "m6", "m6=", "m7", "m8", "m9", "m9=", "m10",
                "m11", "m12", "m12=", "m13",
            ]
        );
    }

    #[test]
    fn index_method_reference_operator_references() {
        let context = index_source({
            "
            X != Y
            X % Y
            X & Y
            X && Y
            X * Y
            X ** Y
            X + Y
            X - Y
            X / Y
            X << Y
            X == Y
            X === Y
            X >> Y
            X ^ Y
            X | Y
            X || Y
            X <=> Y
            "
        });

        assert_diagnostics_eq!(
            &context,
            vec![
                "Warning: possibly useless use of != in void context (1:1-1:7)",
                "Warning: possibly useless use of % in void context (2:1-2:6)",
                "Warning: possibly useless use of & in void context (3:1-3:6)",
                "Warning: possibly useless use of * in void context (5:1-5:6)",
                "Warning: possibly useless use of ** in void context (6:1-6:7)",
                "Warning: possibly useless use of + in void context (7:1-7:6)",
                "Warning: possibly useless use of - in void context (8:1-8:6)",
                "Warning: possibly useless use of / in void context (9:1-9:6)",
                "Warning: possibly useless use of == in void context (11:1-11:7)",
                "Warning: possibly useless use of ^ in void context (14:1-14:6)",
                "Warning: possibly useless use of | in void context (15:1-15:6)",
                "Warning: possibly useless use of <=> in void context (17:1-17:8)"
            ]
        );

        assert_method_references_eq!(
            &context,
            vec![
                "!=", "%", "&", "&&", "*", "**", "+", "-", "/", "<<", "==", "===", ">>", "^", "|", "||", "<=>",
            ]
        );
    }

    #[test]
    fn index_method_reference_lesser_than_operator_references() {
        let context = index_source({
            "
            x < y
            "
        });

        assert_diagnostics_eq!(
            &context,
            vec!["Warning: possibly useless use of < in void context (1:1-1:6)"]
        );

        assert_method_references_eq!(&context, vec!["x", "<", "<=>", "y"]);
    }

    #[test]
    fn index_method_reference_lesser_than_or_equal_to_operator_references() {
        let context = index_source({
            "
            x <= y
            "
        });

        assert_diagnostics_eq!(
            &context,
            vec!["Warning: possibly useless use of <= in void context (1:1-1:7)"]
        );

        assert_method_references_eq!(&context, vec!["x", "<=", "<=>", "y"]);
    }

    #[test]
    fn index_method_reference_greater_than_operator_references() {
        let context = index_source({
            "
            x > y
            "
        });

        assert_diagnostics_eq!(
            &context,
            vec!["Warning: possibly useless use of > in void context (1:1-1:6)"]
        );

        assert_method_references_eq!(&context, vec!["x", "<=>", ">", "y"]);
    }

    #[test]
    fn index_method_reference_greater_than_or_equal_to_operator_references() {
        let context = index_source({
            "
            x >= y
            "
        });

        assert_diagnostics_eq!(
            &context,
            vec!["Warning: possibly useless use of >= in void context (1:1-1:7)"]
        );

        assert_method_references_eq!(&context, vec!["x", "<=>", ">=", "y"]);
    }

    #[test]
    fn index_method_reference_alias_references() {
        let context = index_source({
            "
            alias ignored m1
            alias_method :ignored, :m2
            alias_method :ignored, ignored
            "
        });

        assert_no_diagnostics!(&context);
        assert_method_references_eq!(&context, vec!["m1", "m2"]);
    }

    #[test]
    fn superclasses_are_indexed_as_constant_ref_ids() {
        let context = index_source({
            "
            class Foo < Bar; end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:21", Class, |def| {
            assert_eq!(
                &def.superclass_ref().unwrap(),
                context.graph().constant_references().values().collect::<Vec<_>>()[0].name_id()
            );
        });
    }

    #[test]
    fn constant_path_superclasses() {
        let context = index_source({
            "
            class Foo < Bar::Baz; end
            "
        });

        assert_no_diagnostics!(&context);

        let mut refs = context.graph().constant_references().values().collect::<Vec<_>>();
        refs.sort_by_key(|a| (a.offset().start(), a.offset().end()));

        assert_definition_at!(&context, "1:1-1:26", Class, |def| {
            assert_eq!(&def.superclass_ref().unwrap(), refs[1].name_id());
        });
    }

    #[test]
    fn ignored_super_classes() {
        let context = index_source({
            "
            class Foo < method_call; end
            class Bar < 123; end
            class MyMigration < ActiveRecord::Migration[8.0]; end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:29", Class, |def| {
            assert!(def.superclass_ref().is_none(),);
        });

        assert_definition_at!(&context, "2:1-2:21", Class, |def| {
            assert!(def.superclass_ref().is_none(),);
        });

        assert_definition_at!(&context, "3:1-3:54", Class, |def| {
            assert!(def.superclass_ref().is_none(),);
        });
    }

    #[test]
    fn index_includes_at_top_level() {
        let context = index_source({
            "
            include Bar, Baz, ignored, 123
            include Qux
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 0);
    }

    #[test]
    fn index_includes_in_classes() {
        let context = index_source({
            "
            class Foo
              include Bar, Baz, ignored, 123
              include Qux
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-4:4", Class, |def| {
            assert_includes_eq!(&context, def, vec!["Bar", "Baz", "Qux"]);
        });
    }

    #[test]
    fn index_includes_in_modules() {
        let context = index_source({
            "
            module Foo
              include Bar, Baz, ignored, 123
              include Qux
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-4:4", Module, |def| {
            assert_includes_eq!(&context, def, vec!["Bar", "Baz", "Qux"]);
        });
    }

    #[test]
    fn index_prepends_at_top_level() {
        let context = index_source({
            "
            prepend Bar, Baz, ignored, 123
            prepend Qux
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 0);
    }

    #[test]
    fn index_prepends_in_classes() {
        let context = index_source({
            "
            class Foo
              prepend Bar, Baz, ignored, 123
              prepend Qux
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-4:4", Class, |def| {
            assert_prepends_eq!(&context, def, vec!["Bar", "Baz", "Qux"]);
        });
    }

    #[test]
    fn index_prepends_in_modules() {
        let context = index_source({
            "
            module Foo
              prepend Bar, Baz, ignored, 123
              prepend Qux
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-4:4", Module, |def| {
            assert_prepends_eq!(&context, def, vec!["Bar", "Baz", "Qux"]);
        });
    }

    #[test]
    fn index_extends_in_class() {
        let context = index_source({
            "
            class Foo
              extend Bar
              extend Baz
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-4:4", Class, |class_def| {
            assert_extends_eq!(&context, class_def, vec!["Bar", "Baz"]);
        });
    }

    #[test]
    fn index_mixins_self() {
        let context = index_source({
            "
            module Foo
              include self
              prepend self
              extend self
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-5:4", Module, |def| {
            assert_includes_eq!(&context, def, vec!["Foo"]);
            assert_prepends_eq!(&context, def, vec!["Foo"]);
            assert_extends_eq!(&context, def, vec!["Foo"]);
        });
    }

    #[test]
    fn index_mixins_self_at_top_level() {
        let context = index_source({
            "
            include self
            prepend self
            extend self
            "
        });

        assert_no_diagnostics!(&context);

        assert_eq!(context.graph().definitions().len(), 0);
    }
}
