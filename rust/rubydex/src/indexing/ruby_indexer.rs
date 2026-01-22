//! Visit the Ruby AST and create the definitions.

use crate::diagnostic::Rule;
use crate::indexing::local_graph::LocalGraph;
use crate::model::comment::Comment;
use crate::model::definitions::{
    AttrAccessorDefinition, AttrReaderDefinition, AttrWriterDefinition, ClassDefinition, ClassVariableDefinition,
    ConstantAliasDefinition, ConstantDefinition, Definition, DefinitionFlags, DslDefinition, ExtendDefinition,
    GlobalVariableAliasDefinition, GlobalVariableDefinition, IncludeDefinition, InstanceVariableDefinition,
    MethodAliasDefinition, MethodDefinition, Mixin, ModuleDefinition, Parameter, ParameterStruct, PrependDefinition,
    Receiver, SingletonClassDefinition,
};
use crate::model::document::Document;
use crate::model::dsl::{DslArgument, DslArgumentList, DslValue};
use crate::model::ids::{DefinitionId, NameId, ReferenceId, StringId, UriId};
use crate::model::name::{Name, ParentScope};
use crate::model::references::{ConstantReference, MethodRef};
use crate::model::visibility::Visibility;
use crate::offset::Offset;

use ruby_prism::{ParseResult, Visit};

#[derive(Clone, Copy)]
enum MixinType {
    Include,
    Prepend,
    Extend,
}

enum Nesting {
    /// Nesting stack entries that produce a new lexical scope to which constant references must be attached to (i.e.:
    /// the class and module keywords). All lexical scopes are also owner, but the opposite is not true
    LexicalScope(DefinitionId),
    /// A method entry that is used to set the correct owner for instance variables, but cannot own anything itself
    Method(DefinitionId),
    /// A DSL definition entry that owns its block contents. Members defined inside become members of the DSL.
    /// DSL doesn't create a Ruby lexical scope.
    Dsl(DefinitionId),
}

impl Nesting {
    fn id(&self) -> DefinitionId {
        match self {
            Nesting::LexicalScope(id) | Nesting::Method(id) | Nesting::Dsl(id) => *id,
        }
    }
}

struct VisibilityModifier {
    visibility: Visibility,
    is_inline: bool,
    offset: Offset,
}

impl VisibilityModifier {
    #[must_use]
    pub fn new(visibility: Visibility, is_inline: bool, offset: Offset) -> Self {
        Self {
            visibility,
            is_inline,
            offset,
        }
    }

    #[must_use]
    pub fn visibility(&self) -> &Visibility {
        &self.visibility
    }

    #[must_use]
    pub fn is_inline(&self) -> bool {
        self.is_inline
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }
}

/// Gets the location for just the constant name (not including the namespace or value).
fn constant_name_location<'a>(node: &'a ruby_prism::Node<'a>) -> ruby_prism::Location<'a> {
    match node {
        ruby_prism::Node::ConstantWriteNode { .. } => node.as_constant_write_node().unwrap().name_loc(),
        ruby_prism::Node::ConstantOrWriteNode { .. } => node.as_constant_or_write_node().unwrap().name_loc(),
        ruby_prism::Node::ConstantPathNode { .. } => node.as_constant_path_node().unwrap().name_loc(),
        _ => node.location(),
    }
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
    nesting_stack: Vec<Nesting>,
    visibility_stack: Vec<VisibilityModifier>,
    /// DSL method names to capture as DSL definitions.
    dsl_method_names: Vec<&'static str>,
}

impl<'a> RubyIndexer<'a> {
    #[must_use]
    pub fn new(uri: String, source: &'a str, dsl_method_names: Vec<&'static str>) -> Self {
        let uri_id = UriId::from(&uri);
        let local_graph = LocalGraph::new(uri_id, Document::new(uri, source));

        Self {
            uri_id,
            local_graph,
            source,
            comments: Vec::new(),
            nesting_stack: Vec::new(),
            visibility_stack: vec![VisibilityModifier::new(Visibility::Private, false, Offset::new(0, 0))],
            dsl_method_names,
        }
    }

    /// Checks if a method call matches a DSL target by method name.
    fn is_dsl_target(&self, method_name: &str) -> bool {
        self.dsl_method_names.contains(&method_name)
    }

    /// Gets the `NameId` for a chained constant assignment (e.g., for `A = B = ...`, returns B's `NameId`).
    fn get_chained_constant_name(&mut self, value: &ruby_prism::Node) -> Option<NameId> {
        match value {
            ruby_prism::Node::ConstantWriteNode { .. } => {
                self.index_constant_reference(&value.as_constant_write_node().unwrap().as_node(), false)
            }
            ruby_prism::Node::ConstantOrWriteNode { .. } => {
                self.index_constant_reference(&value.as_constant_or_write_node().unwrap().as_node(), true)
            }
            ruby_prism::Node::ConstantPathWriteNode { .. } => {
                self.index_constant_reference(&value.as_constant_path_write_node().unwrap().target().as_node(), false)
            }
            ruby_prism::Node::ConstantPathOrWriteNode { .. } => self.index_constant_reference(
                &value.as_constant_path_or_write_node().unwrap().target().as_node(),
                true,
            ),
            _ => None,
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
                Rule::ParseError,
                Offset::from_prism_location(&error.location()),
                error.message().to_string(),
            );
        }

        for warning in result.warnings() {
            self.local_graph.add_diagnostic(
                Rule::ParseWarning,
                Offset::from_prism_location(&warning.location()),
                warning.message().to_string(),
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

    /// Parses arguments from a [`CallNode`] into a [`DslArgumentList`].
    /// Constant references are indexed and stored as `ReferenceId`, other values as strings.
    fn parse_dsl_arguments(&mut self, call: &ruby_prism::CallNode) -> DslArgumentList {
        let mut args = DslArgumentList::new();

        if let Some(arguments) = call.arguments() {
            for argument in &arguments.arguments() {
                match argument {
                    ruby_prism::Node::SplatNode { .. } => {
                        let splat = argument.as_splat_node().unwrap();
                        if let Some(expr) = splat.expression() {
                            let text = Self::location_to_string(&expr.location());
                            args.add(DslArgument::Splat(text));
                        }
                    }
                    ruby_prism::Node::KeywordHashNode { .. } => {
                        let hash = argument.as_keyword_hash_node().unwrap();
                        for element in &hash.elements() {
                            match element {
                                ruby_prism::Node::AssocNode { .. } => {
                                    let assoc = element.as_assoc_node().unwrap();
                                    let key = Self::location_to_string(&assoc.key().location());
                                    let key = key.trim_end_matches(':').to_string();
                                    let value_node = assoc.value();
                                    let value = self.parse_dsl_value(&value_node);
                                    args.add(DslArgument::KeywordArg { key, value });
                                }
                                ruby_prism::Node::AssocSplatNode { .. } => {
                                    let splat = element.as_assoc_splat_node().unwrap();
                                    if let Some(expr) = splat.value() {
                                        let text = Self::location_to_string(&expr.location());
                                        args.add(DslArgument::DoubleSplat(text));
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    ruby_prism::Node::BlockArgumentNode { .. } => {
                        let block_arg = argument.as_block_argument_node().unwrap();
                        if let Some(expr) = block_arg.expression() {
                            let text = Self::location_to_string(&expr.location());
                            args.add(DslArgument::BlockArg(text));
                        }
                    }
                    _ => {
                        let value = self.parse_dsl_value(&argument);
                        args.add(DslArgument::Positional(value));
                    }
                }
            }
        }

        if let Some(block) = call.block() {
            args.set_block_offset(Offset::from_prism_location(&block.location()));
        }

        args
    }

    /// Parses a DSL value node into a `DslValue`.
    /// Constant references are indexed and stored as `ReferenceId`, other values as strings.
    fn parse_dsl_value(&mut self, node: &ruby_prism::Node) -> DslValue {
        match node {
            ruby_prism::Node::ConstantReadNode { .. } | ruby_prism::Node::ConstantPathNode { .. } => {
                // Index the constant reference and store the ReferenceId
                if let Some(ref_id) = self.index_constant_reference_for_dsl(node) {
                    DslValue::Reference(ref_id)
                } else {
                    // Fallback to string if indexing fails
                    DslValue::String(Self::location_to_string(&node.location()))
                }
            }
            _ => DslValue::String(Self::location_to_string(&node.location())),
        }
    }

    /// Indexes a constant reference for use in DSL arguments.
    fn index_constant_reference_for_dsl(&mut self, node: &ruby_prism::Node) -> Option<ReferenceId> {
        let name_id = self.index_constant_reference(node, false)?;
        let offset = Offset::from_prism_location(&constant_name_location(node));
        let constant_ref = ConstantReference::new(name_id, self.uri_id, offset);
        Some(self.local_graph.add_constant_reference(constant_ref))
    }

    fn find_comments_for(&self, offset: u32) -> (Vec<Comment>, DefinitionFlags) {
        let offset_usize = offset as usize;
        if self.comments.is_empty() {
            return (Vec::new(), DefinitionFlags::empty());
        }

        let idx = match self.comments.binary_search_by_key(&offset_usize, |g| g.end_offset) {
            Ok(_) => {
                // This should never happen in valid Ruby syntax - a comment cannot end exactly
                // where a definition begins (there must be at least a newline between them)
                debug_assert!(false, "Comment ends exactly at definition start - this indicates a bug");
                return (Vec::new(), DefinitionFlags::empty());
            }
            Err(i) if i > 0 => i - 1,
            Err(_) => return (Vec::new(), DefinitionFlags::empty()),
        };

        let group = &self.comments[idx];
        let between = &self.source.as_bytes()[group.end_offset..offset_usize];
        if !between.iter().all(|&b| b.is_ascii_whitespace()) {
            return (Vec::new(), DefinitionFlags::empty());
        }

        // We allow at most one blank line between the comment and the definition
        if bytecount::count(between, b'\n') > 2 {
            return (Vec::new(), DefinitionFlags::empty());
        }

        (group.comments(), group.flags())
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

    /// Gets the `NameId` of the current lexical scope (class/module/singleton class).
    /// DSL blocks are skipped - use `current_owner_name_id()` for resolving `self` in DSL blocks.
    fn current_lexical_scope_name_id(&self) -> Option<NameId> {
        self.nesting_stack.iter().rev().find_map(|nesting| match nesting {
            Nesting::LexicalScope(id) => {
                if let Some(definition) = self.local_graph.definitions().get(id) {
                    match definition {
                        Definition::Class(class_def) => Some(*class_def.name_id()),
                        Definition::Module(module_def) => Some(*module_def.name_id()),
                        Definition::SingletonClass(singleton_class_def) => Some(*singleton_class_def.name_id()),
                        Definition::Method(_) | Definition::Dsl(_) => None,
                        _ => panic!("current nesting is not a class/module/singleton class: {definition:?}"),
                    }
                } else {
                    None
                }
            }
            Nesting::Dsl(_) | Nesting::Method(_) => None,
        })
    }

    /// Gets the `NameId` of the current owner (class/module/singleton class), including DSL blocks.
    /// Used to resolve `self` in singleton method definitions (e.g., `def self.bar`).
    ///
    /// Unlike `current_lexical_scope_name_id`, this method considers `Nesting::Dsl` entries,
    /// because `self` inside a `Class.new` block refers to the new class being created.
    fn current_owner_name_id(&self) -> Option<NameId> {
        self.nesting_stack.iter().rev().find_map(|nesting| match nesting {
            Nesting::LexicalScope(id) => {
                if let Some(definition) = self.local_graph.definitions().get(id) {
                    match definition {
                        Definition::Class(class_def) => Some(*class_def.name_id()),
                        Definition::Module(module_def) => Some(*module_def.name_id()),
                        Definition::SingletonClass(singleton_class_def) => Some(*singleton_class_def.name_id()),
                        Definition::Method(_) | Definition::Dsl(_) => None,
                        _ => panic!("current nesting is not a class/module/singleton class: {definition:?}"),
                    }
                } else {
                    None
                }
            }
            Nesting::Dsl(id) => {
                // For DSL definitions assigned to a constant (e.g., `Foo = Class.new`),
                // follow the `assigned_to` reference to get the constant's NameId.
                if let Some(Definition::Dsl(dsl)) = self.local_graph.definitions().get(id)
                    && let Some(constant_id) = dsl.assigned_to()
                    && let Some(Definition::Constant(constant)) = self.local_graph.definitions().get(&constant_id)
                {
                    return Some(*constant.name_id());
                }
                None
            }
            Nesting::Method(_) => None,
        })
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
        let mut parent_scope_id = ParentScope::None;

        let location = match node {
            ruby_prism::Node::ConstantPathNode { .. } => {
                let constant = node.as_constant_path_node().unwrap();

                if let Some(parent) = constant.parent() {
                    // Ignore parent scopes that are not constants, like `foo::Bar`
                    match parent {
                        ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. } => {}
                        _ => {
                            self.local_graph.add_diagnostic(
                                Rule::DynamicConstantReference,
                                Offset::from_prism_location(&parent.location()),
                                "Dynamic constant reference".to_string(),
                            );

                            return None;
                        }
                    }

                    parent_scope_id = self
                        .index_constant_reference(&parent, true)
                        .map_or(ParentScope::None, ParentScope::Some);
                } else {
                    parent_scope_id = ParentScope::TopLevel;
                }

                constant.name_loc()
            }
            ruby_prism::Node::ConstantPathWriteNode { .. } => {
                let constant = node.as_constant_path_write_node().unwrap();
                let target = constant.target();

                if let Some(parent) = target.parent() {
                    // Ignore parent scopes that are not constants, like `foo::Bar`
                    match parent {
                        ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. } => {}
                        _ => {
                            return None;
                        }
                    }

                    parent_scope_id = self
                        .index_constant_reference(&parent, true)
                        .map_or(ParentScope::None, ParentScope::Some);
                } else {
                    parent_scope_id = ParentScope::TopLevel;
                }

                target.name_loc()
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

                    parent_scope_id = self
                        .index_constant_reference(&parent, true)
                        .map_or(ParentScope::None, ParentScope::Some);
                } else {
                    parent_scope_id = ParentScope::TopLevel;
                }

                target.name_loc()
            }
            _ => {
                return None;
            }
        };

        let offset = Offset::from_prism_location(&location);
        let name = Self::location_to_string(&location);
        let string_id = self.local_graph.intern_string(name);
        let name_id = self.local_graph.add_name(Name::new(
            string_id,
            parent_scope_id,
            self.current_lexical_scope_name_id(),
        ));

        if push_final_reference {
            self.local_graph
                .add_constant_reference(ConstantReference::new(name_id, self.uri_id, offset));
        }

        Some(name_id)
    }

    fn index_method_reference(&mut self, name: String, location: &ruby_prism::Location, receiver: Option<NameId>) {
        let offset = Offset::from_prism_location(location);
        let str_id = self.local_graph.intern_string(name);
        let reference = MethodRef::new(str_id, self.uri_id, offset, receiver);
        self.local_graph.add_method_reference(reference);
    }

    fn add_definition_from_location<F>(&mut self, location: &ruby_prism::Location, builder: F) -> DefinitionId
    where
        F: FnOnce(StringId, Offset, Vec<Comment>, DefinitionFlags, Option<DefinitionId>, UriId) -> Definition,
    {
        let name = Self::location_to_string(location);
        let str_id = self.local_graph.intern_string(name);
        let offset = Offset::from_prism_location(location);
        let (comments, flags) = self.find_comments_for(offset.start());
        let parent_nesting_id = self.parent_nesting_id();
        let uri_id = self.uri_id;

        let definition = builder(str_id, offset, comments, flags, parent_nesting_id, uri_id);
        let definition_id = self.local_graph.add_definition(definition);

        self.add_member_to_current_owner(definition_id);

        definition_id
    }

    fn add_instance_variable_definition(&mut self, location: &ruby_prism::Location) -> DefinitionId {
        let name = Self::location_to_string(location);
        let str_id = self.local_graph.intern_string(name);
        let offset = Offset::from_prism_location(location);
        let (comments, flags) = self.find_comments_for(offset.start());
        let parent_nesting_id = self.parent_nesting_id();
        let uri_id = self.uri_id;

        let definition = Definition::InstanceVariable(Box::new(InstanceVariableDefinition::new(
            str_id,
            uri_id,
            offset,
            comments,
            flags,
            parent_nesting_id,
        )));

        let definition_id = self.local_graph.add_definition(definition);
        self.add_member_to_current_owner(definition_id);
        definition_id
    }

    /// Adds a class variable definition.
    ///
    /// Class variables use lexical scoping - they belong to the lexically enclosing class/module,
    /// not the method receiver. This is different from instance variables which follow the receiver.
    fn add_class_variable_definition(&mut self, location: &ruby_prism::Location) -> DefinitionId {
        let name = Self::location_to_string(location);
        let str_id = self.local_graph.intern_string(name);
        let offset = Offset::from_prism_location(location);
        let (comments, flags) = self.find_comments_for(offset.start());
        // Class variables use the enclosing class/module (skipping methods) as lexical nesting
        let lexical_nesting_id = self.parent_lexical_scope_id();
        let uri_id = self.uri_id;

        let definition = Definition::ClassVariable(Box::new(ClassVariableDefinition::new(
            str_id,
            uri_id,
            offset,
            comments,
            flags,
            lexical_nesting_id,
        )));

        let definition_id = self.local_graph.add_definition(definition);
        self.add_member_to_current_owner(definition_id);
        definition_id
    }

    fn add_constant_definition(&mut self, node: &ruby_prism::Node, also_add_reference: bool) -> Option<DefinitionId> {
        let name_id = self.index_constant_reference(node, also_add_reference)?;

        let offset = Offset::from_prism_location(&constant_name_location(node));
        let (comments, flags) = self.find_comments_for(offset.start());
        let lexical_nesting_id = self.parent_lexical_scope_id();

        let definition = Definition::Constant(Box::new(ConstantDefinition::new(
            name_id,
            self.uri_id,
            offset,
            comments,
            flags,
            lexical_nesting_id,
        )));
        let definition_id = self.local_graph.add_definition(definition);

        self.add_member_to_current_owner(definition_id);

        Some(definition_id)
    }

    fn handle_class_definition(
        &mut self,
        location: &ruby_prism::Location,
        name_node: Option<&ruby_prism::Node>,
        body_node: Option<ruby_prism::Node>,
        superclass_node: Option<ruby_prism::Node>,
        nesting_type: fn(DefinitionId) -> Nesting,
    ) {
        let offset = Offset::from_prism_location(location);
        let (comments, flags) = self.find_comments_for(offset.start());
        let lexical_nesting_id = self.parent_lexical_scope_id();
        let superclass = superclass_node.as_ref().and_then(|n| {
            self.index_constant_reference(n, false).map(|id| {
                self.local_graph.add_constant_reference(ConstantReference::new(
                    id,
                    self.uri_id,
                    Offset::from_prism_location(&n.location()),
                ))
            })
        });

        if let Some(superclass_node) = superclass_node
            && superclass.is_none()
        {
            self.local_graph.add_diagnostic(
                Rule::DynamicAncestor,
                Offset::from_prism_location(&superclass_node.location()),
                "Dynamic superclass".to_string(),
            );
        }

        let (name_id, name_offset) = if let Some(name_node) = name_node {
            let name_loc = match name_node {
                ruby_prism::Node::ConstantPathNode { .. } => name_node.as_constant_path_node().unwrap().name_loc(),
                ruby_prism::Node::ConstantPathWriteNode { .. } => {
                    name_node.as_constant_path_write_node().unwrap().target().name_loc()
                }
                _ => name_node.location(),
            };
            (
                self.index_constant_reference(name_node, false),
                Offset::from_prism_location(&name_loc),
            )
        } else {
            let string_id = self
                .local_graph
                .intern_string(format!("{}:{}<anonymous>", self.uri_id, offset.start()));

            (
                Some(self.local_graph.add_name(Name::new(string_id, ParentScope::None, None))),
                offset.clone(),
            )
        };

        if let Some(name_id) = name_id {
            let definition = Definition::Class(Box::new(ClassDefinition::new(
                name_id,
                self.uri_id,
                offset.clone(),
                name_offset,
                comments,
                flags,
                lexical_nesting_id,
                superclass,
            )));

            let definition_id = self.local_graph.add_definition(definition);

            self.add_member_to_current_lexical_scope(definition_id);

            if let Some(body) = body_node {
                self.nesting_stack.push(nesting_type(definition_id));
                self.visibility_stack
                    .push(VisibilityModifier::new(Visibility::Public, false, offset));
                self.visit(&body);
                self.visibility_stack.pop();
                self.nesting_stack.pop();
            }
        }
    }

    fn handle_module_definition(
        &mut self,
        location: &ruby_prism::Location,
        name_node: Option<&ruby_prism::Node>,
        body_node: Option<ruby_prism::Node>,
        nesting_type: fn(DefinitionId) -> Nesting,
    ) {
        let offset = Offset::from_prism_location(location);
        let (comments, flags) = self.find_comments_for(offset.start());
        let lexical_nesting_id = self.parent_lexical_scope_id();

        let (name_id, name_offset) = if let Some(name_node) = name_node {
            let name_loc = match name_node {
                ruby_prism::Node::ConstantPathNode { .. } => name_node.as_constant_path_node().unwrap().name_loc(),
                ruby_prism::Node::ConstantPathWriteNode { .. } => {
                    name_node.as_constant_path_write_node().unwrap().target().name_loc()
                }
                _ => name_node.location(),
            };
            (
                self.index_constant_reference(name_node, false),
                Offset::from_prism_location(&name_loc),
            )
        } else {
            let string_id = self
                .local_graph
                .intern_string(format!("{}:{}<anonymous>", self.uri_id, offset.start()));

            (
                Some(self.local_graph.add_name(Name::new(string_id, ParentScope::None, None))),
                offset.clone(),
            )
        };

        if let Some(name_id) = name_id {
            let definition = Definition::Module(Box::new(ModuleDefinition::new(
                name_id,
                self.uri_id,
                offset.clone(),
                name_offset,
                comments,
                flags,
                lexical_nesting_id,
            )));

            let definition_id = self.local_graph.add_definition(definition);

            self.add_member_to_current_lexical_scope(definition_id);

            if let Some(body) = body_node {
                self.nesting_stack.push(nesting_type(definition_id));
                self.visibility_stack
                    .push(VisibilityModifier::new(Visibility::Public, false, offset));
                self.visit(&body);
                self.visibility_stack.pop();
                self.nesting_stack.pop();
            }
        }
    }

    /// Returns the definition ID of the current nesting (class, module, singleton class, or DSL),
    /// but skips methods in the definitions stack.
    fn current_nesting_definition_id(&self) -> Option<DefinitionId> {
        self.nesting_stack.iter().rev().find_map(|nesting| match nesting {
            Nesting::LexicalScope(id) | Nesting::Dsl(id) => Some(*id),
            Nesting::Method(_) => None,
        })
    }

    /// Gets the final constant target from a value, unwrapping chained assignments.
    /// For `A = B = C`, returns C's `NameId`. Returns None if the chain ends in a non-constant.
    fn index_constant_alias_target(&mut self, value: &ruby_prism::Node) -> Option<NameId> {
        match value {
            ruby_prism::Node::ConstantReadNode { .. } | ruby_prism::Node::ConstantPathNode { .. } => {
                self.index_constant_reference(value, true)
            }
            ruby_prism::Node::ConstantWriteNode { .. } => {
                let node = value.as_constant_write_node().unwrap();
                let target_name_id = self.index_constant_alias_target(&node.value())?;
                self.add_constant_alias_definition(value, target_name_id, false);
                Some(target_name_id)
            }
            ruby_prism::Node::ConstantOrWriteNode { .. } => {
                let node = value.as_constant_or_write_node().unwrap();
                let target_name_id = self.index_constant_alias_target(&node.value())?;
                self.add_constant_alias_definition(value, target_name_id, false);
                Some(target_name_id)
            }
            ruby_prism::Node::ConstantPathWriteNode { .. } => {
                let node = value.as_constant_path_write_node().unwrap();
                let target_name_id = self.index_constant_alias_target(&node.value())?;
                self.add_constant_alias_definition(&node.target().as_node(), target_name_id, false);
                Some(target_name_id)
            }
            ruby_prism::Node::ConstantPathOrWriteNode { .. } => {
                let node = value.as_constant_path_or_write_node().unwrap();
                let target_name_id = self.index_constant_alias_target(&node.value())?;
                self.add_constant_alias_definition(&node.target().as_node(), target_name_id, true);
                Some(target_name_id)
            }
            _ => None,
        }
    }

    fn add_constant_alias_definition(
        &mut self,
        name_node: &ruby_prism::Node,
        target_name_id: NameId,
        also_add_reference: bool,
    ) -> Option<DefinitionId> {
        let name_id = self.index_constant_reference(name_node, also_add_reference)?;

        let offset = Offset::from_prism_location(&constant_name_location(name_node));
        let (comments, flags) = self.find_comments_for(offset.start());
        let lexical_nesting_id = self.parent_lexical_scope_id();

        let alias_constant = ConstantDefinition::new(name_id, self.uri_id, offset, comments, flags, lexical_nesting_id);
        let definition =
            Definition::ConstantAlias(Box::new(ConstantAliasDefinition::new(target_name_id, alias_constant)));
        let definition_id = self.local_graph.add_definition(definition);

        self.add_member_to_current_owner(definition_id);

        Some(definition_id)
    }

    /// Indexes a call node as a DSL definition if it matches a DSL target.
    fn try_index_dsl_call(
        &mut self,
        call: &ruby_prism::CallNode,
        assigned_to: Option<DefinitionId>,
    ) -> Option<DefinitionId> {
        let method_name = String::from_utf8_lossy(call.name().as_slice());
        if !self.is_dsl_target(&method_name) {
            return None;
        }

        let method_name_str_id = self.local_graph.intern_string(method_name.to_string());

        let receiver_name_id = call.receiver().and_then(|receiver| match &receiver {
            ruby_prism::Node::ConstantReadNode { .. } | ruby_prism::Node::ConstantPathNode { .. } => {
                self.index_constant_reference(&receiver, true)
            }
            _ => None,
        });

        let offset = Offset::from_prism_location(&call.location());
        let lexical_nesting_id = self.parent_lexical_scope_id();
        let arguments = self.parse_dsl_arguments(call);

        let dsl_def = DslDefinition::new(
            receiver_name_id,
            method_name_str_id,
            arguments,
            self.uri_id,
            offset,
            lexical_nesting_id,
            assigned_to,
        );

        let definition_id = dsl_def.id();
        self.local_graph.add_definition(Definition::Dsl(Box::new(dsl_def)));

        if let Some(block) = call.block() {
            self.nesting_stack.push(Nesting::Dsl(definition_id));
            self.visit(&block);
            self.nesting_stack.pop();
        }

        Some(definition_id)
    }

    /// Creates a constant for DSL assignments, using `parent_nesting_id()` to include DSL parents.
    fn create_constant_definition(
        &mut self,
        name_node: &ruby_prism::Node,
        also_add_reference: bool,
    ) -> Option<DefinitionId> {
        let constant_name = self.index_constant_reference(name_node, also_add_reference)?;

        let offset = Offset::from_prism_location(&constant_name_location(name_node));
        let (comments, flags) = self.find_comments_for(offset.start());
        let lexical_nesting_id = self.parent_nesting_id();

        let constant_def =
            ConstantDefinition::new(constant_name, self.uri_id, offset, comments, flags, lexical_nesting_id);
        let definition_id = constant_def.id();
        self.local_graph
            .add_definition(Definition::Constant(Box::new(constant_def)));

        self.add_member_to_current_owner(definition_id);

        Some(definition_id)
    }

    /// Indexes a constant assignment. Handles DSL calls, aliases, and chained assignments.
    fn index_constant_assignment(
        &mut self,
        name_node: &ruby_prism::Node,
        value: &ruby_prism::Node,
        also_add_reference: bool,
    ) {
        // DSL call (e.g., `A = Class.new`) - create constant first for `class << self` resolution
        if let ruby_prism::Node::CallNode { .. } = value {
            let call = value.as_call_node().unwrap();
            let method_name = String::from_utf8_lossy(call.name().as_slice());
            if self.is_dsl_target(&method_name) {
                let constant_def_id = self.create_constant_definition(name_node, also_add_reference);
                self.try_index_dsl_call(&call, constant_def_id);
                return;
            }
        }

        // Alias to existing constant (e.g., `A = Target` or `A = B = Target`)
        // Note: index_constant_alias_target already creates inner definitions and references,
        // so we don't need to visit(value) here.
        if let Some(target_name_id) = self.index_constant_alias_target(value) {
            self.add_constant_alias_definition(name_node, target_name_id, also_add_reference);
            return;
        }

        // Chained assignment (e.g., `A = B = Class.new`) - A aliases B
        if let Some(inner_name_id) = self.get_chained_constant_name(value) {
            self.visit(value);
            self.add_constant_alias_definition(name_node, inner_name_id, also_add_reference);
            return;
        }

        self.add_constant_definition(name_node, also_add_reference);
        self.visit(value);
    }

    /// Adds a member to the current owner (class, module, singleton class, or DSL).
    fn add_member_to_current_owner(&mut self, member_id: DefinitionId) {
        let Some(owner_id) = self.current_nesting_definition_id() else {
            return;
        };

        let owner = self
            .local_graph
            .get_definition_mut(owner_id)
            .expect("owner definition should exist");

        match owner {
            Definition::Class(class) => class.add_member(member_id),
            Definition::SingletonClass(singleton_class) => singleton_class.add_member(member_id),
            Definition::Module(module) => module.add_member(member_id),
            Definition::Dsl(dsl) => dsl.add_member(member_id),
            _ => unreachable!("find above only matches class/module/singleton/dsl"),
        }
    }

    /// Adds a member to the current lexical scope (class, module, singleton class - excludes DSLs).
    fn add_member_to_current_lexical_scope(&mut self, member_id: DefinitionId) {
        let Some(owner_id) = self.parent_lexical_scope_id() else {
            return;
        };

        let owner = self
            .local_graph
            .get_definition_mut(owner_id)
            .expect("owner definition should exist");

        match owner {
            Definition::Class(class) => class.add_member(member_id),
            Definition::SingletonClass(singleton_class) => singleton_class.add_member(member_id),
            Definition::Module(module) => module.add_member(member_id),
            _ => unreachable!("find above only matches class/module/singleton"),
        }
    }

    fn handle_mixin(&mut self, node: &ruby_prism::CallNode, mixin_type: MixinType) {
        let Some(arguments) = node.arguments() else {
            return;
        };

        let parent_nesting_id = self.current_nesting_definition_id();

        // Collect all arguments as constant references. Ignore anything that isn't a constant
        let mixin_arguments = arguments
            .arguments()
            .iter()
            .filter_map(|arg| {
                if arg.as_self_node().is_some() {
                    if parent_nesting_id.is_none() {
                        self.local_graph.add_diagnostic(
                            Rule::TopLevelMixinSelf,
                            Offset::from_prism_location(&arg.location()),
                            "Top level mixin self".to_string(),
                        );

                        return None;
                    }

                    Some((
                        self.current_lexical_scope_name_id().unwrap(),
                        Offset::from_prism_location(&arg.location()),
                    ))
                } else if let Some(name_id) = self.index_constant_reference(&arg, false) {
                    Some((name_id, Offset::from_prism_location(&arg.location())))
                } else {
                    self.local_graph.add_diagnostic(
                        Rule::DynamicAncestor,
                        Offset::from_prism_location(&arg.location()),
                        "Dynamic mixin argument".to_string(),
                    );

                    None
                }
            })
            .collect::<Vec<(NameId, Offset)>>();

        if mixin_arguments.is_empty() {
            return;
        }

        let Some(lexical_nesting_id) = parent_nesting_id else {
            return;
        };

        // Mixin operations with multiple arguments are inserted in reverse, so that they are processed in the expected
        // order by resolution
        for (id, offset) in mixin_arguments.into_iter().rev() {
            let constant_ref_id =
                self.local_graph
                    .add_constant_reference(ConstantReference::new(id, self.uri_id, offset));

            let mixin = match mixin_type {
                MixinType::Include => Mixin::Include(IncludeDefinition::new(constant_ref_id)),
                MixinType::Prepend => Mixin::Prepend(PrependDefinition::new(constant_ref_id)),
                MixinType::Extend => Mixin::Extend(ExtendDefinition::new(constant_ref_id)),
            };

            match self.local_graph.get_definition_mut(lexical_nesting_id).unwrap() {
                Definition::Class(class_def) => class_def.add_mixin(mixin),
                Definition::Module(module_def) => module_def.add_mixin(mixin),
                Definition::SingletonClass(singleton_class_def) => singleton_class_def.add_mixin(mixin),
                Definition::Dsl(dsl_def) => dsl_def.add_mixin(mixin),
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
    fn parent_lexical_scope_id(&self) -> Option<DefinitionId> {
        self.nesting_stack.iter().rev().find_map(|nesting| match nesting {
            Nesting::LexicalScope(id) => Some(*id),
            Nesting::Method(_) | Nesting::Dsl(_) => None,
        })
    }

    /// Gets the parent for definitions that should be nested under DSLs (like singleton classes).
    /// Unlike `parent_lexical_scope_id`, this includes DSLs that are assigned to a constant.
    fn parent_for_singleton_class(&self) -> Option<DefinitionId> {
        self.nesting_stack.iter().rev().find_map(|nesting| match nesting {
            Nesting::LexicalScope(id) => Some(*id),
            Nesting::Dsl(id) => {
                // Include DSLs assigned to a constant, as they create a namespace for singleton classes.
                if let Some(Definition::Dsl(dsl)) = self.local_graph.definitions().get(id)
                    && dsl.assigned_to().is_some()
                {
                    return Some(*id);
                }
                None
            }
            Nesting::Method(_) => None,
        })
    }

    #[must_use]
    fn parent_nesting_id(&self) -> Option<DefinitionId> {
        self.nesting_stack.last().map(Nesting::id)
    }

    #[must_use]
    fn current_visibility(&self) -> &VisibilityModifier {
        self.visibility_stack
            .last()
            .expect("visibility stack should not be empty")
    }

    fn method_receiver(
        &mut self,
        receiver: Option<&ruby_prism::Node>,
        fallback_location: ruby_prism::Location,
    ) -> Option<NameId> {
        let mut is_singleton_name = false;

        let name_id = match receiver {
            Some(ruby_prism::Node::SelfNode { .. }) | None => {
                // Implicit or explicit self receiver

                match self.nesting_stack.last() {
                    Some(Nesting::LexicalScope(id) | Nesting::Dsl(id)) => {
                        let definition = self
                            .local_graph
                            .definitions()
                            .get(id)
                            .expect("Nesting definition should exist");

                        match definition {
                            Definition::Class(class_def) => Some(*class_def.name_id()),
                            Definition::Module(module_def) => Some(*module_def.name_id()),
                            Definition::SingletonClass(singleton_class_def) => Some(*singleton_class_def.name_id()),
                            Definition::Method(_) | Definition::Dsl(_) => None,
                            _ => panic!("current nesting is not a class/module/singleton class: {definition:?}"),
                        }
                    }
                    Some(Nesting::Method(id)) => {
                        // If we're inside a method definition, we need to check what its receiver is as that changes the type of `self`
                        let Some(Definition::Method(definition)) = self.local_graph.definitions().get(id) else {
                            unreachable!("method definition for nesting should exist")
                        };

                        if let Some(method_def_receiver) = definition.receiver() {
                            is_singleton_name = true;
                            match method_def_receiver {
                                Receiver::ConstantReceiver(name_id) => Some(name_id),
                                Receiver::SelfReceiver(def_id) => {
                                    // Get the name_id from the definition
                                    self.local_graph.definitions().get(&def_id).and_then(|def| match def {
                                        Definition::Class(c) => Some(*c.name_id()),
                                        Definition::Module(m) => Some(*m.name_id()),
                                        Definition::SingletonClass(s) => Some(*s.name_id()),
                                        _ => None,
                                    })
                                }
                            }
                        } else {
                            self.current_owner_name_id()
                        }
                    }
                    None => {
                        let str_id = self.local_graph.intern_string("Object".into());
                        Some(self.local_graph.add_name(Name::new(str_id, ParentScope::None, None)))
                    }
                }
            }
            Some(ruby_prism::Node::CallNode { .. }) => {
                // Check if the receiver is `singleton_class`
                let call_node = receiver.unwrap().as_call_node().unwrap();

                if call_node.name().as_slice() == b"singleton_class" {
                    is_singleton_name = true;
                    self.method_receiver(call_node.receiver().as_ref(), call_node.location())
                } else {
                    None
                }
            }
            Some(node) => {
                is_singleton_name = true;
                self.index_constant_reference(node, true)
            }
        }?;

        if !is_singleton_name {
            return Some(name_id);
        }

        let singleton_class_name = {
            let name = self
                .local_graph
                .names()
                .get(&name_id)
                .expect("Indexed constant name should exist");

            let target_str = self
                .local_graph
                .strings()
                .get(name.str())
                .expect("Indexed constant string should exist");

            format!("<{}>", target_str.as_str())
        };

        let string_id = self.local_graph.intern_string(singleton_class_name);
        let new_name_id = self
            .local_graph
            .add_name(Name::new(string_id, ParentScope::Attached(name_id), None));

        let location = receiver.map_or(fallback_location, ruby_prism::Node::location);
        let offset = Offset::from_prism_location(&location);
        self.local_graph
            .add_constant_reference(ConstantReference::new(new_name_id, self.uri_id, offset));
        Some(new_name_id)
    }
}

struct CommentGroup {
    end_offset: usize,
    comments: Vec<Comment>,
    deprecated: bool,
}

impl CommentGroup {
    #[must_use]
    pub fn new() -> Self {
        Self {
            end_offset: 0,
            comments: Vec::new(),
            deprecated: false,
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

        if text.lines().any(|line| line.starts_with("# @deprecated")) {
            self.deprecated = true;
        }

        self.comments.push(Comment::new(
            Offset::from_prism_location(&comment.location()),
            text.trim().to_string(),
        ));
    }

    fn comments(&self) -> Vec<Comment> {
        self.comments.clone()
    }

    fn flags(&self) -> DefinitionFlags {
        if self.deprecated {
            DefinitionFlags::DEPRECATED
        } else {
            DefinitionFlags::empty()
        }
    }
}

impl Visit<'_> for RubyIndexer<'_> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'_>) {
        self.handle_class_definition(
            &node.location(),
            Some(&node.constant_path()),
            node.body(),
            node.superclass(),
            Nesting::LexicalScope,
        );
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode) {
        self.handle_module_definition(
            &node.location(),
            Some(&node.constant_path()),
            node.body(),
            Nesting::LexicalScope,
        );
    }

    fn visit_singleton_class_node(&mut self, node: &ruby_prism::SingletonClassNode) {
        let expression = node.expression();

        // Determine the attached_target for the singleton class and the name_offset
        let (attached_target, name_offset) = if expression.as_self_node().is_some() {
            // `class << self` - resolve self to current class/module's NameId
            // For DSL blocks (e.g., `Foo = Class.new do...end`), use the constant's NameId
            // name_offset points to "self"
            (
                self.current_owner_name_id(),
                Offset::from_prism_location(&expression.location()),
            )
        } else if matches!(
            expression,
            ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. }
        ) {
            // `class << Foo` or `class << Foo::Bar` - use the constant's NameId
            // name_offset points to the expression (the constant reference)
            (
                self.index_constant_reference(&expression, true),
                Offset::from_prism_location(&expression.location()),
            )
        } else {
            // Dynamic expression (e.g., `class << some_var`) - skip creating definition
            self.visit(&expression);
            self.local_graph.add_diagnostic(
                Rule::DynamicSingletonDefinition,
                Offset::from_prism_location(&node.location()),
                "Dynamic singleton class definition".to_string(),
            );
            return;
        };

        let Some(attached_target) = attached_target else {
            self.local_graph.add_diagnostic(
                Rule::DynamicSingletonDefinition,
                Offset::from_prism_location(&node.location()),
                "Dynamic singleton class definition".to_string(),
            );

            return;
        };

        let offset = Offset::from_prism_location(&node.location());
        let (comments, flags) = self.find_comments_for(offset.start());
        let lexical_nesting_id = self.parent_for_singleton_class();

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
            format!("<{}>", target_str.as_str())
        };

        let string_id = self.local_graph.intern_string(singleton_class_name);
        let name_id = self
            .local_graph
            .add_name(Name::new(string_id, ParentScope::Attached(attached_target), None));

        let definition = Definition::SingletonClass(Box::new(SingletonClassDefinition::new(
            name_id,
            self.uri_id,
            offset.clone(),
            name_offset,
            comments,
            flags,
            lexical_nesting_id,
        )));

        let definition_id = self.local_graph.add_definition(definition);

        self.add_member_to_current_owner(definition_id);

        if let Some(body) = node.body() {
            self.nesting_stack.push(Nesting::LexicalScope(definition_id));
            self.visibility_stack
                .push(VisibilityModifier::new(Visibility::Public, false, offset));
            self.visit(&body);
            self.visibility_stack.pop();
            self.nesting_stack.pop();
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
        self.index_constant_assignment(&node.as_node(), &node.value(), true);
    }

    fn visit_constant_write_node(&mut self, node: &ruby_prism::ConstantWriteNode) {
        self.index_constant_assignment(&node.as_node(), &node.value(), false);
    }

    fn visit_constant_path_and_write_node(&mut self, node: &ruby_prism::ConstantPathAndWriteNode) {
        self.visit_constant_path_node(&node.target());
        self.visit(&node.value());
    }

    fn visit_constant_path_operator_write_node(&mut self, node: &ruby_prism::ConstantPathOperatorWriteNode) {
        self.visit_constant_path_node(&node.target());
        self.visit(&node.value());
    }

    fn visit_constant_path_or_write_node(&mut self, node: &ruby_prism::ConstantPathOrWriteNode) {
        self.index_constant_assignment(&node.target().as_node(), &node.value(), true);
    }

    fn visit_constant_path_write_node(&mut self, node: &ruby_prism::ConstantPathWriteNode) {
        self.index_constant_assignment(&node.target().as_node(), &node.value(), false);
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
                        |str_id, offset, comments, flags, lexical_nesting_id, uri_id| {
                            Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                                str_id,
                                uri_id,
                                offset,
                                comments,
                                flags,
                                lexical_nesting_id,
                            )))
                        },
                    );
                }
                ruby_prism::Node::InstanceVariableTargetNode { .. } => {
                    self.add_instance_variable_definition(&left.location());
                }
                ruby_prism::Node::ClassVariableTargetNode { .. } => {
                    self.add_class_variable_definition(&left.location());
                }
                ruby_prism::Node::CallTargetNode { .. } => {
                    let call_target_node = left.as_call_target_node().unwrap();
                    let method_receiver = self.method_receiver(Some(&call_target_node.receiver()), left.location());

                    if method_receiver.is_none() {
                        self.visit(&call_target_node.receiver());
                    }

                    let name = String::from_utf8_lossy(call_target_node.name().as_slice()).to_string();
                    self.index_method_reference(name, &call_target_node.location(), method_receiver);
                }
                _ => {}
            }
        }

        self.visit(&node.value());
    }

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode) {
        let name = Self::location_to_string(&node.name_loc());
        let str_id = self.local_graph.intern_string(format!("{name}()"));
        let offset = Offset::from_prism_location(&node.location());
        let parent_nesting_id = self.current_nesting_definition_id();
        let parameters = self.collect_parameters(node);
        let is_singleton = node.receiver().is_some();

        let current_visibility = self.current_visibility();
        let (visibility, offset_for_comments) = if is_singleton {
            (Visibility::Public, offset.clone())
        } else if current_visibility.is_inline() {
            // If the visibility is inline, we use its offset for the comments
            (*current_visibility.visibility(), current_visibility.offset().clone())
        } else {
            (*current_visibility.visibility(), offset.clone())
        };

        let (comments, flags) = self.find_comments_for(offset_for_comments.start());

        let receiver: Option<Receiver> = if let Some(recv_node) = node.receiver() {
            match recv_node {
                // def self.foo - receiver is the enclosing class/module/DSL definition
                ruby_prism::Node::SelfNode { .. } => self.current_nesting_definition_id().map(Receiver::SelfReceiver),
                // def Foo.bar or def Foo::Bar.baz - receiver is an explicit constant
                ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. } => self
                    .index_constant_reference(&recv_node, true)
                    .map(Receiver::ConstantReceiver),
                // Dynamic receiver (def foo.bar) - visit and then skip
                // We still want to visit because it could be a variable reference
                _ => {
                    self.local_graph.add_diagnostic(
                        Rule::DynamicSingletonDefinition,
                        Offset::from_prism_location(&node.location()),
                        "Dynamic receiver for singleton method definition".to_string(),
                    );

                    self.visit(&recv_node);
                    return;
                }
            }
        } else {
            None
        };

        let definition_id = if receiver.is_none() && visibility == Visibility::ModuleFunction {
            // module_function creates two method definitions:
            // 1. Public singleton method (class/module method)
            let self_receiver = self.current_nesting_definition_id().map(Receiver::SelfReceiver);
            let method = Definition::Method(Box::new(MethodDefinition::new(
                str_id,
                self.uri_id,
                offset.clone(),
                comments.clone(),
                flags.clone(),
                parent_nesting_id,
                parameters.clone(),
                Visibility::Public,
                self_receiver,
            )));
            let definition_id = self.local_graph.add_definition(method);

            self.add_member_to_current_owner(definition_id);

            // 2. Private instance method
            let method = Definition::Method(Box::new(MethodDefinition::new(
                str_id,
                self.uri_id,
                offset,
                comments,
                flags,
                parent_nesting_id,
                parameters,
                Visibility::Private,
                receiver,
            )));
            let definition_id = self.local_graph.add_definition(method);

            self.add_member_to_current_owner(definition_id);

            definition_id
        } else {
            let method = Definition::Method(Box::new(MethodDefinition::new(
                str_id,
                self.uri_id,
                offset,
                comments,
                flags,
                parent_nesting_id,
                parameters,
                visibility,
                receiver,
            )));
            let definition_id = self.local_graph.add_definition(method);

            self.add_member_to_current_owner(definition_id);

            definition_id
        };

        if let Some(body) = node.body() {
            self.nesting_stack.push(Nesting::Method(definition_id));
            self.visit(&body);
            self.nesting_stack.pop();
        }
    }

    #[allow(clippy::too_many_lines)]
    fn visit_call_node(&mut self, node: &ruby_prism::CallNode) {
        enum AttrKind {
            Accessor,
            Reader,
            Writer,
        }

        // Check if this is a standalone DSL call (e.g., Class.new do...end)
        // DSL calls assigned to constants are handled in visit_constant_write_node
        if self.try_index_dsl_call(node, None).is_some() {
            return;
        }

        let mut index_attr = |kind: AttrKind, call: &ruby_prism::CallNode| {
            let call_offset = Offset::from_prism_location(&call.location());

            Self::each_string_or_symbol_arg(call, |name, location| {
                let str_id = self.local_graph.intern_string(format!("{name}()"));
                let parent_nesting_id = self.parent_nesting_id();
                let offset = Offset::from_prism_location(&location);

                let current_visibility = self.current_visibility();
                let (visibility, offset_for_comments) = if current_visibility.is_inline() {
                    // If the visibility is inline, we use its offset for the comments
                    (*current_visibility.visibility(), current_visibility.offset().clone())
                } else {
                    (*current_visibility.visibility(), call_offset.clone())
                };

                let (comments, flags) = self.find_comments_for(offset_for_comments.start());

                // module_function makes attr_* methods private (without creating singleton methods)
                let visibility = match visibility {
                    Visibility::ModuleFunction => Visibility::Private,
                    v => v,
                };

                let definition = match kind {
                    AttrKind::Accessor => Definition::AttrAccessor(Box::new(AttrAccessorDefinition::new(
                        str_id,
                        self.uri_id,
                        offset,
                        comments,
                        flags,
                        parent_nesting_id,
                        visibility,
                    ))),
                    AttrKind::Reader => Definition::AttrReader(Box::new(AttrReaderDefinition::new(
                        str_id,
                        self.uri_id,
                        offset,
                        comments,
                        flags,
                        parent_nesting_id,
                        visibility,
                    ))),
                    AttrKind::Writer => Definition::AttrWriter(Box::new(AttrWriterDefinition::new(
                        str_id,
                        self.uri_id,
                        offset,
                        comments,
                        flags,
                        parent_nesting_id,
                        visibility,
                    ))),
                };

                let definition_id = self.local_graph.add_definition(definition);
                self.add_member_to_current_owner(definition_id);
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
                let mut names: Vec<(String, Offset)> = Vec::new();

                Self::each_string_or_symbol_arg(node, |name, location| {
                    names.push((name, Offset::from_prism_location(&location)));
                });

                if names.len() != 2 {
                    // TODO: Add a diagnostic for this
                    return;
                }

                let (new_name, _new_offset) = &names[0];
                let (old_name, old_offset) = &names[1];

                let new_name_str_id = self.local_graph.intern_string(format!("{new_name}()"));
                let old_name_str_id = self.local_graph.intern_string(format!("{old_name}()"));

                let method_receiver = self.method_receiver(node.receiver().as_ref(), node.location());
                let reference = MethodRef::new(old_name_str_id, self.uri_id, old_offset.clone(), method_receiver);
                self.local_graph.add_method_reference(reference);

                let offset = Offset::from_prism_location(&node.location());
                let (comments, flags) = self.find_comments_for(offset.start());

                let definition = Definition::MethodAlias(Box::new(MethodAliasDefinition::new(
                    new_name_str_id,
                    old_name_str_id,
                    self.uri_id,
                    offset,
                    comments,
                    flags,
                    self.current_nesting_definition_id(),
                )));

                let definition_id = self.local_graph.add_definition(definition);

                self.add_member_to_current_owner(definition_id);
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
            "private" | "protected" | "public" | "module_function" => {
                if let Some(_receiver) = node.receiver() {
                    self.visit_call_node_parts(node);
                    return;
                }

                let visibility = Visibility::from_string(message.as_str());
                let offset = Offset::from_prism_location(&node.location());

                if let Some(arguments) = node.arguments() {
                    // With this case:
                    //
                    // ```ruby
                    // private def foo(bar); end
                    // ```
                    //
                    // We push the new visibility to the stack and then pop it after visiting the arguments so it only affects the method definition.
                    self.visibility_stack
                        .push(VisibilityModifier::new(visibility, true, offset));
                    self.visit_arguments_node(&arguments);
                    self.visibility_stack.pop();
                } else {
                    // With this case:
                    //
                    // ```ruby
                    // private
                    //
                    // def foo(bar); end
                    // ```
                    //
                    // We replace the current visibility with the new one so it only affects all the subsequent method definitions.
                    let last_visibility = self.visibility_stack.last_mut().unwrap();
                    *last_visibility = VisibilityModifier::new(visibility, false, offset);
                }
            }
            _ => {
                // For method calls that we don't explicitly handle each part, we continue visiting their parts as we
                // may discover something inside
                if let Some(arguments) = node.arguments() {
                    self.visit_arguments_node(&arguments);
                }

                if let Some(block) = node.block() {
                    self.visit(&block);
                }

                let method_receiver = self.method_receiver(node.receiver().as_ref(), node.location());

                if method_receiver.is_none()
                    && let Some(receiver) = node.receiver()
                {
                    self.visit(&receiver);
                }

                self.index_method_reference(message.clone(), &node.message_loc().unwrap(), method_receiver);

                match message.as_str() {
                    ">" | "<" | ">=" | "<=" => {
                        self.index_method_reference("<=>".to_string(), &node.message_loc().unwrap(), method_receiver);
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

        let method_receiver = self.method_receiver(node.receiver().as_ref(), node.location());
        let read_name = String::from_utf8_lossy(node.read_name().as_slice()).to_string();
        self.index_method_reference(read_name, &node.operator_loc(), method_receiver);

        let write_name = String::from_utf8_lossy(node.write_name().as_slice()).to_string();
        self.index_method_reference(write_name, &node.operator_loc(), method_receiver);

        self.visit(&node.value());
    }

    fn visit_call_operator_write_node(&mut self, node: &ruby_prism::CallOperatorWriteNode) {
        if let Some(receiver) = node.receiver() {
            self.visit(&receiver);
        }

        let method_receiver = self.method_receiver(node.receiver().as_ref(), node.location());
        let read_name = String::from_utf8_lossy(node.read_name().as_slice()).to_string();
        self.index_method_reference(read_name, &node.call_operator_loc().unwrap(), method_receiver);

        let write_name = String::from_utf8_lossy(node.write_name().as_slice()).to_string();
        self.index_method_reference(write_name, &node.call_operator_loc().unwrap(), method_receiver);

        self.visit(&node.value());
    }

    fn visit_call_or_write_node(&mut self, node: &ruby_prism::CallOrWriteNode) {
        if let Some(receiver) = node.receiver() {
            self.visit(&receiver);
        }

        let method_receiver = self.method_receiver(node.receiver().as_ref(), node.location());
        let read_name = String::from_utf8_lossy(node.read_name().as_slice()).to_string();
        self.index_method_reference(read_name, &node.operator_loc(), method_receiver);

        let write_name = String::from_utf8_lossy(node.write_name().as_slice()).to_string();
        self.index_method_reference(write_name, &node.operator_loc(), method_receiver);

        self.visit(&node.value());
    }

    fn visit_global_variable_write_node(&mut self, node: &ruby_prism::GlobalVariableWriteNode) {
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, flags, lexical_nesting_id, uri_id| {
                Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                    str_id,
                    uri_id,
                    offset,
                    comments,
                    flags,
                    lexical_nesting_id,
                )))
            },
        );
        self.visit(&node.value());
    }

    fn visit_global_variable_and_write_node(&mut self, node: &ruby_prism::GlobalVariableAndWriteNode<'_>) {
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, flags, nesting_id, uri_id| {
                Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                    str_id, uri_id, offset, comments, flags, nesting_id,
                )))
            },
        );
        self.visit(&node.value());
    }

    fn visit_global_variable_or_write_node(&mut self, node: &ruby_prism::GlobalVariableOrWriteNode<'_>) {
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, flags, nesting_id, uri_id| {
                Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                    str_id, uri_id, offset, comments, flags, nesting_id,
                )))
            },
        );
        self.visit(&node.value());
    }

    fn visit_global_variable_operator_write_node(&mut self, node: &ruby_prism::GlobalVariableOperatorWriteNode<'_>) {
        self.add_definition_from_location(
            &node.name_loc(),
            |str_id, offset, comments, flags, nesting_id, uri_id| {
                Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
                    str_id, uri_id, offset, comments, flags, nesting_id,
                )))
            },
        );
        self.visit(&node.value());
    }

    fn visit_instance_variable_and_write_node(&mut self, node: &ruby_prism::InstanceVariableAndWriteNode) {
        self.add_instance_variable_definition(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_instance_variable_operator_write_node(&mut self, node: &ruby_prism::InstanceVariableOperatorWriteNode) {
        self.add_instance_variable_definition(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_instance_variable_or_write_node(&mut self, node: &ruby_prism::InstanceVariableOrWriteNode) {
        self.add_instance_variable_definition(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_instance_variable_write_node(&mut self, node: &ruby_prism::InstanceVariableWriteNode) {
        self.add_instance_variable_definition(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_class_variable_and_write_node(&mut self, node: &ruby_prism::ClassVariableAndWriteNode) {
        self.add_class_variable_definition(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_class_variable_operator_write_node(&mut self, node: &ruby_prism::ClassVariableOperatorWriteNode) {
        self.add_class_variable_definition(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_class_variable_or_write_node(&mut self, node: &ruby_prism::ClassVariableOrWriteNode) {
        self.add_class_variable_definition(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_class_variable_write_node(&mut self, node: &ruby_prism::ClassVariableWriteNode) {
        self.add_class_variable_definition(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_block_argument_node(&mut self, node: &ruby_prism::BlockArgumentNode<'_>) {
        let expression = node.expression();
        if let Some(expression) = expression {
            match expression {
                ruby_prism::Node::SymbolNode { .. } => {
                    let symbol = expression.as_symbol_node().unwrap();
                    let name = Self::location_to_string(&symbol.value_loc().unwrap());
                    self.index_method_reference(name, &node.location(), None);
                }
                _ => {
                    self.visit(&expression);
                }
            }
        }
    }

    fn visit_alias_method_node(&mut self, node: &ruby_prism::AliasMethodNode<'_>) {
        let mut new_name = if let Some(symbol_node) = node.new_name().as_symbol_node() {
            Self::location_to_string(&symbol_node.value_loc().unwrap())
        } else {
            Self::location_to_string(&node.new_name().location())
        };

        let mut old_name = if let Some(symbol_node) = node.old_name().as_symbol_node() {
            Self::location_to_string(&symbol_node.value_loc().unwrap())
        } else {
            Self::location_to_string(&node.old_name().location())
        };

        new_name.push_str("()");
        old_name.push_str("()");

        let offset = Offset::from_prism_location(&node.location());
        let (comments, flags) = self.find_comments_for(offset.start());
        let definition = Definition::MethodAlias(Box::new(MethodAliasDefinition::new(
            self.local_graph.intern_string(new_name),
            self.local_graph.intern_string(old_name.clone()),
            self.uri_id,
            offset,
            comments,
            flags,
            self.current_nesting_definition_id(),
        )));

        let definition_id = self.local_graph.add_definition(definition);

        self.add_member_to_current_owner(definition_id);
        self.index_method_reference(old_name, &node.old_name().location(), None);
    }

    fn visit_alias_global_variable_node(&mut self, node: &ruby_prism::AliasGlobalVariableNode<'_>) {
        let new_name = Self::location_to_string(&node.new_name().location());
        let old_name = Self::location_to_string(&node.old_name().location());
        let offset = Offset::from_prism_location(&node.location());
        let (comments, flags) = self.find_comments_for(offset.start());

        let definition = Definition::GlobalVariableAlias(Box::new(GlobalVariableAliasDefinition::new(
            self.local_graph.intern_string(new_name),
            self.local_graph.intern_string(old_name),
            self.uri_id,
            offset,
            comments,
            flags,
            self.parent_nesting_id(),
        )));

        let definition_id = self.local_graph.add_definition(definition);

        self.add_member_to_current_owner(definition_id);
    }

    fn visit_and_node(&mut self, node: &ruby_prism::AndNode) {
        let left = node.left();
        let method_receiver = self.method_receiver(Some(&left), left.location());

        if method_receiver.is_none() {
            self.visit(&left);
        }

        self.index_method_reference("&&".to_string(), &node.location(), method_receiver);
        self.visit(&node.right());
    }

    fn visit_or_node(&mut self, node: &ruby_prism::OrNode) {
        let left = node.left();
        let method_receiver = self.method_receiver(Some(&left), left.location());

        if method_receiver.is_none() {
            self.visit(&left);
        }

        self.index_method_reference("||".to_string(), &node.location(), method_receiver);
        self.visit(&node.right());
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        model::{
            definitions::{Definition, Mixin, Parameter, Receiver},
            dsl::DslArgument,
            ids::StringId,
            visibility::Visibility,
        },
        test_utils::LocalGraphTest,
    };

    // Primitive assertions

    /// Asserts that a `NameId` resolves to the expected full path string.
    ///
    /// Usage:
    /// - `assert_name_path_eq!(ctx, "Foo::Bar::Baz", name_id)` - asserts the full path `Foo::Bar::Baz`
    /// - `assert_name_path_eq!(ctx, "Baz", name_id)` - asserts just `Baz` with no parent scope
    macro_rules! assert_name_path_eq {
        ($context:expr, $expect_path:expr, $name_id:expr) => {{
            let mut name_parts = Vec::new();
            let mut current_name_id = Some($name_id);

            while let Some(name_id) = current_name_id {
                let name = $context.graph().names().get(&name_id).unwrap();
                name_parts.push($context.graph().strings().get(name.str()).unwrap().as_str());
                current_name_id = name.parent_scope().as_ref().copied();
            }

            name_parts.reverse();

            let actual_path = name_parts.join("::");
            assert_eq!(
                $expect_path, actual_path,
                "name path mismatch: expected `{}`, got `{}`",
                $expect_path, actual_path
            );
        }};
    }

    /// Asserts that a `StringId` resolves to the expected string.
    ///
    /// Usage:
    /// - `assert_string_eq!(ctx, str_id, "Foo::Bar::Baz")`
    macro_rules! assert_string_eq {
        ($context:expr, $str_id:expr, $expected_str:expr) => {{
            let string_name = $context.graph().strings().get($str_id).unwrap().as_str();
            assert_eq!(
                string_name, $expected_str,
                "string mismatch: expected `{}`, got `{}`",
                $expected_str, string_name
            );
        }};
    }

    // Definition assertions

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

        ($context:expr, $location:expr, $variant:ident) => {{
            let __def = $context.definition_at($location);
            let __kind = __def.kind();
            match __def {
                Definition::$variant(_) => {}
                _ => panic!("expected {} definition, got {:?}", stringify!($variant), __kind),
            }
        }};
    }

    /// Asserts that a definition's comments matches the expected comments.
    ///
    /// Usage:
    /// - `assert_def_comments_eq!(ctx, def, ["# Comment 1", "# Comment 2"])`
    macro_rules! assert_def_comments_eq {
        ($context:expr, $def:expr, $expected_comments:expr) => {{
            let actual_comments: Vec<String> = $def.comments().iter().map(|c| c.string().to_string()).collect();
            assert_eq!(
                $expected_comments,
                actual_comments.as_slice(),
                "comments mismatch: expected `{:?}`, got `{:?}`",
                $expected_comments,
                actual_comments
            );
        }};
    }

    /// Asserts that a method has the expected receiver.
    ///
    /// Usage:
    /// - `assert_method_has_receiver!(ctx, method, "Foo")` for constant receiver
    /// - `assert_method_has_receiver!(ctx, method, "Bar")` for self receiver
    macro_rules! assert_method_has_receiver {
        ($context:expr, $method:expr, $expected_receiver:expr) => {{
            use crate::model::definitions::Receiver;
            if let Some(receiver) = $method.receiver() {
                let actual_name = match receiver {
                    Receiver::SelfReceiver(def_id) => {
                        // Get the definition's name
                        let def = $context.graph().definitions().get(&def_id).unwrap();
                        let name_id = match def {
                            crate::model::definitions::Definition::Class(c) => *c.name_id(),
                            crate::model::definitions::Definition::Module(m) => *m.name_id(),
                            crate::model::definitions::Definition::SingletonClass(s) => *s.name_id(),
                            _ => panic!("unexpected definition type for SelfReceiver"),
                        };
                        let name = $context.graph().names().get(&name_id).unwrap();
                        $context.graph().strings().get(name.str()).unwrap().as_str().to_string()
                    }
                    Receiver::ConstantReceiver(name_id) => {
                        let name = $context.graph().names().get(&name_id).unwrap();
                        $context.graph().strings().get(name.str()).unwrap().as_str().to_string()
                    }
                };
                assert_eq!($expected_receiver, actual_name);
            } else {
                panic!("expected method to have receiver, got None");
            }
        }};
    }

    /// Asserts that a definition's mixins matches the expected mixins.
    ///
    /// Usage:
    /// - `assert_def_mixins_eq!(ctx, def, Include, ["Foo", "Bar"])`
    macro_rules! assert_def_mixins_eq {
        ($context:expr, $def:expr, $mixin_type:ident, $expected_names:expr) => {{
            let actual_names = $def
                .mixins()
                .iter()
                .filter_map(|mixin| {
                    if let Mixin::$mixin_type(def) = mixin {
                        let name = $context
                            .graph()
                            .names()
                            .get(
                                $context
                                    .graph()
                                    .constant_references()
                                    .get(def.constant_reference_id())
                                    .unwrap()
                                    .name_id(),
                            )
                            .unwrap();
                        Some($context.graph().strings().get(name.str()).unwrap().as_str())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            assert_eq!(
                $expected_names,
                actual_names.as_slice(),
                "mixins mismatch: expected `{:?}`, got `{:?}`",
                $expected_names,
                actual_names
            );
        }};
    }

    /// Asserts the full path of a definition's `name_id` matches the expected string.
    /// Works with any definition that has a `name_id()` method.
    ///
    /// Usage:
    /// - `assert_def_name_eq!(ctx, "Foo::Bar::Baz", def)` - asserts the full path `Foo::Bar::Baz`
    /// - `assert_def_name_eq!(ctx, "Baz", def)` - asserts just `Baz` with no parent scope
    macro_rules! assert_def_name_eq {
        ($context:expr, $def:expr, $expect_path:expr) => {{
            assert_name_path_eq!($context, $expect_path, *$def.name_id());
        }};
    }

    /// Asserts that a definition's name offset matches the expected location.
    ///
    /// Usage:
    /// - `assert_def_name_offset_eq!(ctx, "1:7-1:10", def)`
    macro_rules! assert_def_name_offset_eq {
        ($context:expr, $def:expr, $expected_location:expr) => {{
            let (_, expected_offset) = $context.parse_location(&format!("{}:{}", $context.uri(), $expected_location));
            assert_eq!(
                &expected_offset,
                $def.name_offset(),
                "name_offset mismatch: expected `{}`, got `{}`",
                expected_offset.to_display_range($context.graph().document()),
                $def.name_offset().to_display_range($context.graph().document())
            );
        }};
    }

    /// Asserts that a definition's string matches the expected string.
    ///
    /// Usage:
    /// - `assert_def_str_eq!(ctx, "baz()", def)`
    macro_rules! assert_def_str_eq {
        ($context:expr, $def:expr, $expect_name_string:expr) => {{
            assert_string_eq!($context, $def.str_id(), $expect_name_string);
        }};
    }

    /// Asserts that a definition's superclass reference matches the expected name.
    ///
    /// Usage:
    /// - `assert_def_superclass_ref_eq!(ctx, def, "Bar::Baz")`
    macro_rules! assert_def_superclass_ref_eq {
        ($context:expr, $def:expr, $expected_name:expr) => {{
            let name = $context
                .graph()
                .strings()
                .get(
                    $context
                        .graph()
                        .names()
                        .get(
                            $context
                                .graph()
                                .constant_references()
                                .get($def.superclass_ref().unwrap())
                                .unwrap()
                                .name_id(),
                        )
                        .unwrap()
                        .str(),
                )
                .unwrap()
                .as_str();
            assert_eq!(
                $expected_name, name,
                "superclass reference mismatch: expected `{}`, got `{name}`",
                $expected_name,
            );
        }};
    }

    macro_rules! assert_block_offset_eq {
        ($context:expr, $expected_location:expr, $dsl_args:expr) => {{
            let (_, expected_offset) = $context.parse_location(&format!("{}:{}", $context.uri(), $expected_location));
            let actual_offset = $dsl_args.block_offset().expect("expected block offset to be set");
            assert_eq!(
                &expected_offset,
                actual_offset,
                "block_offset mismatch: expected {}, got {}",
                expected_offset.to_display_range($context.graph().document()),
                actual_offset.to_display_range($context.graph().document())
            );
        }};
    }

    // Method assertions

    /// Asserts that a parameter matches the expected kind.
    ///
    /// Usage:
    /// - `assert_parameter!(parameter, RequiredPositional, |param| { assert_string_eq!(context, param.str(), "a"); })`
    /// - `assert_parameter!(parameter, OptionalPositional, |param| { assert_string_eq!(context, param.str(), "b"); })`
    macro_rules! assert_parameter {
        ($expr:expr, $variant:ident, |$param:ident| $body:block) => {
            match $expr {
                Parameter::$variant($param) => $body,
                _ => panic!(
                    "parameter kind mismatch: expected `{}`, got `{:?}`",
                    stringify!($variant),
                    $expr
                ),
            }
        };
    }

    // Reference assertions

    macro_rules! assert_constant_references_eq {
        ($context:expr, $expected_names:expr) => {{
            let mut actual_references = $context
                .graph()
                .constant_references()
                .values()
                .map(|r| {
                    let name = $context.graph().names().get(r.name_id()).unwrap();
                    (
                        r.offset().start(),
                        $context.graph().strings().get(name.str()).unwrap().as_str(),
                    )
                })
                .collect::<Vec<_>>();

            actual_references.sort();

            let actual_names = actual_references.iter().map(|(_, name)| *name).collect::<Vec<_>>();

            assert_eq!(
                $expected_names,
                actual_names.as_slice(),
                "constant references mismatch: expected `{:?}`, got `{:?}`",
                $expected_names,
                actual_names
            );
        }};
    }

    macro_rules! assert_method_references_eq {
        ($context:expr, $expected_names:expr) => {{
            let mut actual_references = $context
                .graph()
                .method_references()
                .values()
                .map(|m| {
                    (
                        m.offset().start(),
                        $context.graph().strings().get(m.str()).unwrap().as_str(),
                    )
                })
                .collect::<Vec<_>>();

            actual_references.sort();

            let actual_names = actual_references
                .iter()
                .map(|(_offset, name)| *name)
                .collect::<Vec<_>>();

            assert_eq!(
                $expected_names,
                actual_names.as_slice(),
                "method references mismatch: expected `{:?}`, got `{:?}`",
                $expected_names,
                actual_names
            );
        }};
    }

    // Diagnostic assertions

    fn format_diagnostics(context: &LocalGraphTest) -> Vec<String> {
        let mut diagnostics = context.graph().diagnostics().iter().collect::<Vec<_>>();

        diagnostics.sort_by_key(|d| d.offset());
        diagnostics
            .iter()
            .map(|d| d.formatted(context.graph().document()))
            .collect()
    }

    macro_rules! assert_diagnostics_eq {
        ($context:expr, $expected_diagnostics:expr) => {{
            assert_eq!(
                $expected_diagnostics,
                format_diagnostics($context).as_slice(),
                "diagnostics mismatch: expected `{:?}`, got `{:?}`",
                $expected_diagnostics,
                format_diagnostics($context)
            );
        }};
    }

    macro_rules! assert_no_diagnostics {
        ($context:expr) => {{
            assert!(
                $context.graph().diagnostics().is_empty(),
                "expected no diagnostics, got {:?}",
                format_diagnostics($context)
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
            [
                "parse-error: expected an `end` to close the `class` statement (1:1-1:6)",
                "parse-error: unexpected end-of-input, assuming it is closing the parent top level context (1:10-2:1)"
            ]
        );

        // We still index the definition, even though it has errors
        assert_eq!(context.graph().definitions().len(), 1);
        assert_definition_at!(&context, "1:1-2:1", Class, |def| {
            assert_def_name_eq!(&context, def, "Foo");
        });
    }

    #[test]
    fn index_source_with_warnings() {
        let context = index_source({
            "
            foo = 42
            "
        });

        assert_diagnostics_eq!(
            &context,
            ["parse-warning: assigned but unused variable - foo (1:1-1:4)"]
        );
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
            assert_def_name_eq!(&context, def, "Foo");
            assert_def_name_offset_eq!(&context, def, "1:7-1:10");
            assert!(def.superclass_ref().is_none());
            assert_eq!(1, def.members().len());
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:3-4:6", Class, |def| {
            assert_def_name_eq!(&context, def, "Bar");
            assert_def_name_offset_eq!(&context, def, "2:9-2:12");
            assert!(def.superclass_ref().is_none());
            assert_eq!(1, def.members().len());

            assert_definition_at!(&context, "1:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "3:5-3:19", Class, |def| {
            assert_def_name_eq!(&context, def, "Baz");
            assert_def_name_offset_eq!(&context, def, "3:11-3:14");
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
            assert_def_name_eq!(&context, def, "Foo::Bar");
            assert_def_name_offset_eq!(&context, def, "1:12-1:15");
            assert!(def.superclass_ref().is_none());
            assert!(def.lexical_nesting_id().is_none());
            assert_eq!(1, def.members().len());
        });

        assert_definition_at!(&context, "2:3-4:6", Class, |def| {
            assert_def_name_eq!(&context, def, "Baz::Qux");
            assert_def_name_offset_eq!(&context, def, "2:14-2:17");
            assert!(def.superclass_ref().is_none());
            assert_eq!(1, def.members().len());

            assert_definition_at!(&context, "1:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "3:5-3:23", Class, |def| {
            assert_def_name_eq!(&context, def, "Quuux");
            assert_def_name_offset_eq!(&context, def, "3:13-3:18");
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

        assert_diagnostics_eq!(
            &context,
            ["dynamic-constant-reference: Dynamic constant reference (1:7-1:10)"]
        );
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
            assert_def_name_eq!(&context, def, "Foo");
            assert_def_name_offset_eq!(&context, def, "1:8-1:11");
            assert_eq!(1, def.members().len());
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:3-4:6", Module, |def| {
            assert_def_name_eq!(&context, def, "Bar");
            assert_def_name_offset_eq!(&context, def, "2:10-2:13");
            assert_eq!(1, def.members().len());

            assert_definition_at!(&context, "1:1-5:4", Module, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "3:5-3:20", Module, |def| {
            assert_def_name_eq!(&context, def, "Baz");
            assert_def_name_offset_eq!(&context, def, "3:12-3:15");

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
            assert_def_name_eq!(&context, def, "Foo::Bar");
            assert_def_name_offset_eq!(&context, def, "1:13-1:16");
            assert_eq!(1, def.members().len());
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:3-4:6", Module, |def| {
            assert_def_name_eq!(&context, def, "Baz::Qux");
            assert_def_name_offset_eq!(&context, def, "2:15-2:18");
            assert_eq!(1, def.members().len());

            assert_definition_at!(&context, "1:1-5:4", Module, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "3:5-3:24", Module, |def| {
            assert_def_name_eq!(&context, def, "Quuux");
            assert_def_name_offset_eq!(&context, def, "3:14-3:19");

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

        assert_diagnostics_eq!(
            &context,
            ["dynamic-constant-reference: Dynamic constant reference (1:8-1:11)"]
        );
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
            assert_def_name_eq!(&context, def, "FOO");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:3-4:6", Constant, |def| {
            assert_def_name_eq!(&context, def, "FOO");

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

        assert_definition_at!(&context, "1:6-1:9", Constant, |def| {
            assert_def_name_eq!(&context, def, "FOO::BAR");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:8-4:11", Constant, |def| {
            assert_def_name_eq!(&context, def, "FOO::BAR");

            assert_definition_at!(&context, "3:1-6:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "5:5-5:8", Constant, |def| {
            assert_def_name_eq!(&context, def, "BAZ");

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
            assert_def_name_eq!(&context, def, "FOO");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:3-4:6", Constant, |def| {
            assert_def_name_eq!(&context, def, "BAZ");

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_constant_references_eq!(&context, ["FOO", "BAZ"]);
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

        assert_definition_at!(&context, "1:6-1:9", Constant, |def| {
            assert_def_name_eq!(&context, def, "FOO::BAR");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:8-4:11", Constant, |def| {
            assert_def_name_eq!(&context, def, "FOO::BAR");

            assert_definition_at!(&context, "3:1-6:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "5:5-5:8", Constant, |def| {
            assert_def_name_eq!(&context, def, "BAZ");

            assert_definition_at!(&context, "3:1-6:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[1], def.id());
            });
        });

        assert_constant_references_eq!(&context, ["FOO", "BAR", "FOO", "BAR", "BAZ"]);
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
            assert_def_name_eq!(&context, def, "FOO");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "1:6-1:14", Constant, |def| {
            assert_def_name_eq!(&context, def, "BAR::BAZ");
        });

        assert_definition_at!(&context, "4:3-4:6", Constant, |def| {
            assert_def_name_eq!(&context, def, "FOO");

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "4:8-4:16", Constant, |def| {
            assert_def_name_eq!(&context, def, "BAR::BAZ");

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[1], def.id());
            });
        });

        assert_definition_at!(&context, "4:18-4:23", Constant, |def| {
            assert_def_name_eq!(&context, def, "BAZ");

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
            assert_def_str_eq!(&context, def, "foo()");
            assert_eq!(def.parameters().len(), 0);
            assert!(def.receiver().is_none());
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "3:1-6:4", Class, |foo_class_def| {
            assert_definition_at!(&context, "4:3-4:15", Method, |bar_def| {
                assert_def_str_eq!(&context, bar_def, "bar()");
                assert_eq!(bar_def.parameters().len(), 0);
                assert!(bar_def.receiver().is_none());
                assert_eq!(foo_class_def.id(), bar_def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[0], bar_def.id());
            });

            assert_definition_at!(&context, "5:3-5:20", Method, |baz_def| {
                assert_def_str_eq!(&context, baz_def, "baz()");
                assert_eq!(baz_def.parameters().len(), 0);
                assert_method_has_receiver!(&context, baz_def, "Foo");
                assert_eq!(foo_class_def.id(), baz_def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[1], baz_def.id());
            });
        });

        assert_definition_at!(&context, "8:1-10:4", Class, |bar_class_def| {
            assert_def_name_eq!(&context, bar_class_def, "Bar");

            assert_definition_at!(&context, "9:3-9:19", Method, |quz_def| {
                assert_def_str_eq!(&context, quz_def, "quz()");
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

        assert_diagnostics_eq!(
            &context,
            ["dynamic-singleton-definition: Dynamic receiver for singleton method definition (1:1-1:17)"]
        );
        assert_eq!(context.graph().definitions().len(), 0);
        assert_method_references_eq!(&context, ["foo"]);
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
            assert_def_name_eq!(&context, bar_class, "Bar");
            assert_def_name_offset_eq!(&context, bar_class, "1:7-1:10");
        });

        // class Foo
        assert_definition_at!(&context, "3:1-15:4", Class, |foo_class| {
            assert_def_name_eq!(&context, foo_class, "Foo");
            assert_def_name_offset_eq!(&context, foo_class, "3:7-3:10");

            // class << self (inside Foo)
            assert_definition_at!(&context, "4:3-14:6", SingletonClass, |foo_singleton| {
                assert_def_name_eq!(&context, foo_singleton, "Foo::<Foo>");
                // name_offset points to "self"
                assert_def_name_offset_eq!(&context, foo_singleton, "4:12-4:16");
                assert_eq!(foo_singleton.lexical_nesting_id(), &Some(foo_class.id()));

                // def baz (inside class << self)
                assert_definition_at!(&context, "5:5-5:17", Method, |baz_method| {
                    assert_eq!(baz_method.lexical_nesting_id(), &Some(foo_singleton.id()));
                });

                // class << Bar (inside class << self of Foo)
                assert_definition_at!(&context, "7:5-9:8", SingletonClass, |bar_singleton| {
                    assert_def_name_eq!(&context, bar_singleton, "Bar::<Bar>");
                    // name_offset points to "Bar"
                    assert_def_name_offset_eq!(&context, bar_singleton, "7:14-7:17");
                    assert_eq!(bar_singleton.lexical_nesting_id(), &Some(foo_singleton.id()));

                    // def self.qux (inside class << Bar)
                    assert_definition_at!(&context, "8:7-8:24", Method, |qux_method| {
                        assert_eq!(qux_method.lexical_nesting_id(), &Some(bar_singleton.id()));
                        assert_method_has_receiver!(&context, qux_method, "<Bar>");
                    });
                });

                // class << self (nested inside outer class << self)
                assert_definition_at!(&context, "11:5-13:8", SingletonClass, |nested_singleton| {
                    assert_def_name_eq!(&context, nested_singleton, "Foo::<Foo>::<<Foo>>");
                    // name_offset points to "self"
                    assert_def_name_offset_eq!(&context, nested_singleton, "11:14-11:18");
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
            assert_def_name_eq!(&context, class_def, "Foo::Bar");
            assert_definition_at!(&context, "2:3-4:6", SingletonClass, |singleton_class| {
                assert_eq!(singleton_class.lexical_nesting_id(), &Some(class_def.id()));
                assert_definition_at!(&context, "3:5-3:17", Method, |method| {
                    assert_eq!(method.lexical_nesting_id(), &Some(singleton_class.id()));
                });
            });
        });

        assert_constant_references_eq!(&context, ["Foo"]);
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
                    assert_def_name_eq!(&context, def, "A");
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

        assert_diagnostics_eq!(
            &context,
            ["dynamic-singleton-definition: Dynamic singleton class definition (1:1-3:4)"]
        );
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
                assert_def_str_eq!(&context, def, "@@var");
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
                assert_def_str_eq!(&context, def, "@@var");
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
                assert_def_str_eq!(&context, def, "@@var");
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
            assert_def_str_eq!(&context, def, "m1()");
            assert_eq!(def.visibility(), &Visibility::Private);
        });

        assert_definition_at!(&context, "3:11-3:22", Method, |def| {
            assert_def_str_eq!(&context, def, "m2()");
            assert_eq!(def.visibility(), &Visibility::Protected);
        });

        assert_definition_at!(&context, "7:1-7:12", Method, |def| {
            assert_def_str_eq!(&context, def, "m3()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });
    }

    #[test]
    fn index_module_function() {
        let context = index_source({
            "
            module Foo
              def bar; end

              module_function

              def baz; end
              attr_reader :attribute

              public

              def qux; end

              module_function def boop; end

              def zip; end
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "2:3-2:15", Method, |def| {
            assert_def_str_eq!(&context, def, "bar()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        let definitions = context.all_definitions_at("6:3-6:15");
        assert_eq!(
            definitions.len(),
            2,
            "module_function should create two definitions for baz"
        );

        let instance_method = definitions
            .iter()
            .find(|d| matches!(d, Definition::Method(m) if m.receiver().is_none()))
            .expect("should have instance method definition");
        let Definition::Method(instance_method) = instance_method else {
            panic!()
        };
        assert_def_str_eq!(&context, instance_method, "baz()");
        assert_eq!(instance_method.visibility(), &Visibility::Private);

        let singleton_method = definitions
            .iter()
            .find(|d| matches!(d, Definition::Method(m) if m.receiver().is_some()))
            .expect("should have singleton method definition");
        let Definition::Method(singleton_method) = singleton_method else {
            panic!()
        };
        assert_def_str_eq!(&context, singleton_method, "baz()");
        assert_eq!(singleton_method.visibility(), &Visibility::Public);

        assert_definition_at!(&context, "7:16-7:25", AttrReader, |def| {
            assert_def_str_eq!(&context, def, "attribute()");
            assert_eq!(def.visibility(), &Visibility::Private);
        });

        assert_definition_at!(&context, "11:3-11:15", Method, |def| {
            assert_def_str_eq!(&context, def, "qux()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        let definitions = context.all_definitions_at("13:19-13:32");
        assert_eq!(
            definitions.len(),
            2,
            "module_function should create two definitions for boop"
        );

        let instance_method = definitions
            .iter()
            .find(|d| matches!(d, Definition::Method(m) if m.receiver().is_none()))
            .expect("boop: should have instance method definition");
        let Definition::Method(instance_method) = instance_method else {
            panic!()
        };
        assert_def_str_eq!(&context, instance_method, "boop()");
        assert_eq!(instance_method.visibility(), &Visibility::Private);

        let singleton_method = definitions
            .iter()
            .find(|d| matches!(d, Definition::Method(m) if m.receiver().is_some()))
            .expect("boop: should have singleton method definition");
        let Definition::Method(singleton_method) = singleton_method else {
            panic!()
        };
        assert_def_str_eq!(&context, singleton_method, "boop()");
        assert_eq!(singleton_method.visibility(), &Visibility::Public);

        assert_definition_at!(&context, "15:3-15:15", Method, |def| {
            assert_def_str_eq!(&context, def, "zip()");
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
            assert_def_str_eq!(&context, def, "m1()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "9:5-9:16", Method, |def| {
            assert_def_str_eq!(&context, def, "m2()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "13:5-13:16", Method, |def| {
            assert_def_str_eq!(&context, def, "m3()");
            assert_eq!(def.visibility(), &Visibility::Private);
        });

        assert_definition_at!(&context, "18:3-18:14", Method, |def| {
            assert_def_str_eq!(&context, def, "m4()");
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
            assert_def_str_eq!(&context, def, "m1()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "5:11-5:27", Method, |def| {
            assert_def_str_eq!(&context, def, "m2()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "10:3-10:19", Method, |def| {
            assert_def_str_eq!(&context, def, "m3()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });
    }

    #[test]
    fn index_visibility_in_singleton_class() {
        let context = index_source({
            "
            class Foo
              protected

              class << self
                def m1; end

                private

                def m2; end
              end

              def m3; end
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "5:5-5:16", Method, |def| {
            assert_def_str_eq!(&context, def, "m1()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "9:5-9:16", Method, |def| {
            assert_def_str_eq!(&context, def, "m2()");
            assert_eq!(def.visibility(), &Visibility::Private);
        });

        assert_definition_at!(&context, "12:3-12:14", Method, |def| {
            assert_def_str_eq!(&context, def, "m3()");
            assert_eq!(def.visibility(), &Visibility::Protected);
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
            assert_def_str_eq!(&context, def, "foo()");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:18-4:21", AttrAccessor, |def| {
            assert_def_str_eq!(&context, def, "bar()");

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "4:24-4:27", AttrAccessor, |def| {
            assert_def_str_eq!(&context, def, "baz()");

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
            assert_def_str_eq!(&context, def, "foo()");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:16-4:19", AttrReader, |def| {
            assert_def_str_eq!(&context, def, "bar()");

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "4:22-4:25", AttrReader, |def| {
            assert_def_str_eq!(&context, def, "baz()");

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
            assert_def_str_eq!(&context, def, "foo()");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:16-4:19", AttrWriter, |def| {
            assert_def_str_eq!(&context, def, "bar()");

            assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "4:22-4:25", AttrWriter, |def| {
            assert_def_str_eq!(&context, def, "baz()");

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
            assert_def_str_eq!(&context, def, "a1()");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "1:13-1:15", AttrReader, |def| {
            assert_def_str_eq!(&context, def, "a2()");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:8-4:12", AttrAccessor, |def| {
            assert_def_str_eq!(&context, def, "a3()");

            assert_definition_at!(&context, "3:1-7:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "5:9-5:11", AttrReader, |def| {
            assert_def_str_eq!(&context, def, "a4()");

            assert_definition_at!(&context, "3:1-7:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[1], def.id());
            });
        });

        assert_definition_at!(&context, "6:9-6:11", AttrReader, |def| {
            assert_def_str_eq!(&context, def, "a5()");
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
            assert_def_str_eq!(&context, def, "foo()");
            assert_eq!(def.visibility(), &Visibility::Private);
        });

        assert_definition_at!(&context, "3:24-3:27", AttrReader, |def| {
            assert_def_str_eq!(&context, def, "bar()");
            assert_eq!(def.visibility(), &Visibility::Protected);
        });

        assert_definition_at!(&context, "7:14-7:17", AttrWriter, |def| {
            assert_def_str_eq!(&context, def, "baz()");
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

                public
              end

              attr_writer :qux
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "4:18-4:21", AttrAccessor, |def| {
            assert_def_str_eq!(&context, def, "foo()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "9:20-9:23", AttrAccessor, |def| {
            assert_def_str_eq!(&context, def, "bar()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "13:18-13:21", AttrReader, |def| {
            assert_def_str_eq!(&context, def, "baz()");
            assert_eq!(def.visibility(), &Visibility::Private);
        });

        assert_definition_at!(&context, "18:16-18:19", AttrWriter, |def| {
            assert_def_str_eq!(&context, def, "qux()");
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
            "
        });

        assert_no_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 8);

        assert_definition_at!(&context, "1:1-1:5", GlobalVariable, |def| {
            assert_def_str_eq!(&context, def, "$foo");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:1-2:5", GlobalVariable, |def| {
            assert_def_str_eq!(&context, def, "$bar");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:7-2:11", GlobalVariable, |def| {
            assert_def_str_eq!(&context, def, "$baz");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "5:3-5:7", GlobalVariable, |def| {
            assert_def_str_eq!(&context, def, "$qux");

            assert_definition_at!(&context, "4:1-6:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });

        assert_definition_at!(&context, "8:1-8:5", GlobalVariable, |def| {
            assert_def_str_eq!(&context, def, "$one");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "9:1-9:5", GlobalVariable, |def| {
            assert_def_str_eq!(&context, def, "$two");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "10:1-10:7", GlobalVariable, |def| {
            assert_def_str_eq!(&context, def, "$three");
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
            assert_def_str_eq!(&context, def, "@foo");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "3:1-6:4", Class, |foo_class_def| {
            assert_definition_at!(&context, "4:3-4:7", InstanceVariable, |def| {
                assert_def_str_eq!(&context, def, "@bar");
                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[0], def.id());
            });

            assert_definition_at!(&context, "5:3-5:7", InstanceVariable, |def| {
                assert_def_str_eq!(&context, def, "@baz");
                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[1], def.id());
            });

            assert_definition_at!(&context, "5:9-5:13", InstanceVariable, |def| {
                assert_def_str_eq!(&context, def, "@qux");
                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[2], def.id());
            });
        });

        assert_definition_at!(&context, "8:1-8:5", InstanceVariable, |def| {
            assert_def_str_eq!(&context, def, "@bar");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "9:1-9:5", InstanceVariable, |def| {
            assert_def_str_eq!(&context, def, "@baz");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "10:1-10:5", InstanceVariable, |def| {
            assert_def_str_eq!(&context, def, "@qux");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "12:1-16:4", Class, |bar_class_def| {
            assert_definition_at!(&context, "13:3-13:7", InstanceVariable, |def| {
                assert_def_str_eq!(&context, def, "@foo");
                assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(bar_class_def.members()[0], def.id());
            });

            assert_definition_at!(&context, "14:3-14:7", InstanceVariable, |def| {
                assert_def_str_eq!(&context, def, "@bar");
                assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(bar_class_def.members()[1], def.id());
            });

            assert_definition_at!(&context, "15:3-15:7", InstanceVariable, |def| {
                assert_def_str_eq!(&context, def, "@baz");
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

              def set_foo
                @@foo = 4
              end
            end
            "
        });

        // This is actually not allowed in Ruby and will raise a runtime error
        // But we should still index it so we can insert a diagnostic for it
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:6", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@foo");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "3:1-6:4", Class, |foo_class_def| {
            assert_definition_at!(&context, "4:3-4:8", ClassVariable, |def| {
                assert_def_str_eq!(&context, def, "@@bar");
                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[0], def.id());
            });

            assert_definition_at!(&context, "5:3-5:8", ClassVariable, |def| {
                assert_def_str_eq!(&context, def, "@@baz");
                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[1], def.id());
            });

            assert_definition_at!(&context, "5:10-5:15", ClassVariable, |def| {
                assert_def_str_eq!(&context, def, "@@qux");
                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(foo_class_def.members()[2], def.id());
            });
        });

        assert_definition_at!(&context, "8:1-8:6", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@bar");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "9:1-9:6", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@baz");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "10:1-10:6", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@qux");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "12:1-20:4", Class, |bar_class_def| {
            assert_definition_at!(&context, "13:3-13:8", ClassVariable, |def| {
                assert_def_str_eq!(&context, def, "@@foo");
                assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(bar_class_def.members()[0], def.id());
            });

            assert_definition_at!(&context, "14:3-14:8", ClassVariable, |def| {
                assert_def_str_eq!(&context, def, "@@bar");
                assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(bar_class_def.members()[1], def.id());
            });

            assert_definition_at!(&context, "15:3-15:8", ClassVariable, |def| {
                assert_def_str_eq!(&context, def, "@@baz");
                assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(bar_class_def.members()[2], def.id());
            });

            // Method `set_foo` is members()[3], class variable inside method is members()[4]
            assert_definition_at!(&context, "18:5-18:10", ClassVariable, |def| {
                assert_def_str_eq!(&context, def, "@@foo");
                assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(bar_class_def.members()[4], def.id());
            });
        });
    }

    #[test]
    fn index_unresolved_constant_references() {
        let context = index_source({
            r##"
            puts C1
            puts C2::C3::C4
            puts ignored0::IGNORED0
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
            puts "#{C23}"

            ::IGNORED2 = 42 # IGNORED2 is an assignment
            puts "IGNORED3"
            puts :IGNORED4
            "##
        });

        assert_diagnostics_eq!(
            &context,
            [
                "dynamic-constant-reference: Dynamic constant reference (3:6-3:14)",
                "parse-warning: assigned but unused variable - foo (5:1-5:4)",
            ]
        );

        assert_constant_references_eq!(
            &context,
            [
                "C1", "C2", "C3", "C4", "<C6>", "C6", "C7", "<C8>", "C8", "C9", "C10", "C11", "<C12>", "C12", "C13",
                "C14", "<C16>", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23"
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
            [
                "C1", "C2", "C3", "<C4>", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "<C12>", "C12", "C13",
                "C14"
            ]
        );
    }

    #[test]
    fn index_constant_path_and_write_visits_value() {
        let context = index_source({
            "
            C1::C2 &&= C3
            C4::C5 += C6
            C7::C8 ||= C9
            "
        });

        assert_no_diagnostics!(&context);

        assert_constant_references_eq!(&context, ["C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9"]);
    }

    #[test]
    fn index_unresolved_constant_references_for_classes() {
        let context = index_source({
            "
            C1.new

            class IGNORED < ::C2; end
            class IGNORED < C3; end
            class IGNORED < C4::C5; end
            class IGNORED < ::C7::C6; end

            class C8::IGNORED; end
            class ::C9::IGNORED; end
            class C10::C11::IGNORED; end
            "
        });

        assert_no_diagnostics!(&context);

        assert_constant_references_eq!(
            &context,
            ["C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11"]
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

            module M20
              include self
            end

            module M21
              extend self
            end

            module M22
              prepend self
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_constant_references_eq!(
            &context,
            [
                "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12", "M13", "M14", "M15", "M16",
                "M17", "M18", "M19", "M20", "M21", "M22",
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
            ["parse-error: unexpected ... when the parent method is not forwarding (31:5-31:8)"]
        );

        assert_method_references_eq!(
            &context,
            [
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
            [
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
            [
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
            [
                "parse-warning: possibly useless use of != in void context (1:1-1:7)",
                "parse-warning: possibly useless use of % in void context (2:1-2:6)",
                "parse-warning: possibly useless use of & in void context (3:1-3:6)",
                "parse-warning: possibly useless use of * in void context (5:1-5:6)",
                "parse-warning: possibly useless use of ** in void context (6:1-6:7)",
                "parse-warning: possibly useless use of + in void context (7:1-7:6)",
                "parse-warning: possibly useless use of - in void context (8:1-8:6)",
                "parse-warning: possibly useless use of / in void context (9:1-9:6)",
                "parse-warning: possibly useless use of == in void context (11:1-11:7)",
                "parse-warning: possibly useless use of ^ in void context (14:1-14:6)",
                "parse-warning: possibly useless use of | in void context (15:1-15:6)",
                "parse-warning: possibly useless use of <=> in void context (17:1-17:8)"
            ]
        );

        assert_method_references_eq!(
            &context,
            [
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
            ["parse-warning: possibly useless use of < in void context (1:1-1:6)"]
        );

        assert_method_references_eq!(&context, ["x", "<", "<=>", "y"]);
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
            ["parse-warning: possibly useless use of <= in void context (1:1-1:7)"]
        );

        assert_method_references_eq!(&context, ["x", "<=", "<=>", "y"]);
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
            ["parse-warning: possibly useless use of > in void context (1:1-1:6)"]
        );

        assert_method_references_eq!(&context, ["x", "<=>", ">", "y"]);
    }

    #[test]
    fn index_method_reference_constant_receiver() {
        let context = index_source({
            "
            Foo.bar
            "
        });

        assert_no_diagnostics!(&context);

        let method_ref = context.graph().method_references().values().next().unwrap();
        assert_eq!(StringId::from("bar"), *method_ref.str());

        let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
        assert_eq!(StringId::from("<Foo>"), *receiver.str());
        assert!(receiver.nesting().is_none());

        let parent_scope = context
            .graph()
            .names()
            .get(&receiver.parent_scope().expect("Should exist"))
            .unwrap();
        assert_eq!(StringId::from("Foo"), *parent_scope.str());
        assert!(parent_scope.nesting().is_none());
        assert!(parent_scope.parent_scope().is_none());
    }

    #[test]
    fn index_method_reference_self_receiver() {
        let context = index_source({
            "
            class Foo
              def bar
                baz
              end

              def baz
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        let method_ref = context.graph().method_references().values().next().unwrap();
        assert_eq!(StringId::from("baz"), *method_ref.str());

        let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
        assert_eq!(StringId::from("Foo"), *receiver.str());
        assert!(receiver.nesting().is_none());
        assert!(receiver.parent_scope().is_none());
    }

    #[test]
    fn index_method_reference_explicit_self_receiver() {
        let context = index_source({
            "
            class Foo
              def bar
                self.baz
              end

              def baz
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        let method_ref = context.graph().method_references().values().next().unwrap();
        assert_eq!(StringId::from("baz"), *method_ref.str());

        let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
        assert_eq!(StringId::from("Foo"), *receiver.str());
        assert!(receiver.nesting().is_none());
        assert!(receiver.parent_scope().is_none());
    }

    #[test]
    fn index_method_reference_self_receiver_in_method_ref_with_receiver() {
        let context = index_source({
            "
            class Foo
              def Bar.bar
                baz
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        let method_ref = context.graph().method_references().values().next().unwrap();
        assert_eq!(StringId::from("baz"), *method_ref.str());

        let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
        assert_eq!(StringId::from("<Bar>"), *receiver.str());
        assert!(receiver.nesting().is_none());

        let parent_scope = context
            .graph()
            .names()
            .get(&receiver.parent_scope().expect("Should exist"))
            .unwrap();
        assert_eq!(StringId::from("Bar"), *parent_scope.str());
        assert!(parent_scope.parent_scope().is_none());

        let nesting = context.graph().names().get(&parent_scope.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Foo"), *nesting.str());
        assert!(nesting.nesting().is_none());
        assert!(nesting.parent_scope().is_none());
    }

    #[test]
    fn index_method_reference_self_receiver_in_singleton_method() {
        let context = index_source({
            "
            class Foo
              def self.bar
                baz
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        let method_ref = context.graph().method_references().values().next().unwrap();
        assert_eq!(StringId::from("baz"), *method_ref.str());

        let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
        assert_eq!(StringId::from("<Foo>"), *receiver.str());
        assert!(receiver.nesting().is_none());

        let parent_scope = context
            .graph()
            .names()
            .get(&receiver.parent_scope().expect("Should exist"))
            .unwrap();
        assert_eq!(StringId::from("Foo"), *parent_scope.str());
        assert!(parent_scope.parent_scope().is_none());
        assert!(parent_scope.nesting().is_none());
    }

    #[test]
    fn index_method_reference_empty_message() {
        // Indexing this method reference is necessary for triggering the creation of the singleton class and its
        // ancestor linearization, which then triggers correct completion
        let context = index_source({
            "
            Foo.
            "
        });

        let method_ref = context.graph().method_references().values().next().unwrap();
        assert_eq!(StringId::from(""), *method_ref.str());

        let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
        assert_eq!(StringId::from("<Foo>"), *receiver.str());
        assert!(receiver.nesting().is_none());

        let parent_scope = context
            .graph()
            .names()
            .get(&receiver.parent_scope().expect("Should exist"))
            .unwrap();
        assert_eq!(StringId::from("Foo"), *parent_scope.str());
        assert!(parent_scope.nesting().is_none());
        assert!(parent_scope.parent_scope().is_none());
    }

    #[test]
    fn index_method_reference_singleton_class_receiver() {
        let context = index_source({
            "
            Foo.singleton_class.bar
            "
        });

        assert_no_diagnostics!(&context);

        let method_ref = context.graph().method_references().values().next().unwrap();
        assert_eq!(StringId::from("bar"), *method_ref.str());

        let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
        assert_eq!(StringId::from("<<Foo>>"), *receiver.str(),);
        assert!(receiver.nesting().is_none());

        let singleton = context
            .graph()
            .names()
            .get(&receiver.parent_scope().expect("Should exist"))
            .unwrap();
        assert_eq!(StringId::from("<Foo>"), *singleton.str());
        assert!(singleton.nesting().is_none());

        let attached = context
            .graph()
            .names()
            .get(&singleton.parent_scope().expect("Should exist"))
            .unwrap();
        assert_eq!(StringId::from("Foo"), *attached.str());
        assert!(attached.nesting().is_none());
        assert!(attached.parent_scope().is_none());
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
            ["parse-warning: possibly useless use of >= in void context (1:1-1:7)"]
        );

        assert_method_references_eq!(&context, ["x", "<=>", ">=", "y"]);
    }

    #[test]
    fn index_method_reference_and_node_constant_receiver() {
        let context = index_source({
            "
            Foo && bar
            "
        });

        assert_no_diagnostics!(&context);

        let method_ref = context
            .graph()
            .method_references()
            .values()
            .find(|r| *r.str() == StringId::from("&&"))
            .unwrap();

        let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
        assert_eq!(StringId::from("<Foo>"), *receiver.str());
        assert!(receiver.nesting().is_none());

        let parent_scope = context
            .graph()
            .names()
            .get(&receiver.parent_scope().expect("Should exist"))
            .unwrap();
        assert_eq!(StringId::from("Foo"), *parent_scope.str());
        assert!(parent_scope.nesting().is_none());
        assert!(parent_scope.parent_scope().is_none());
    }

    #[test]
    fn index_method_reference_or_node_constant_receiver() {
        let context = index_source({
            "
            Foo || bar
            "
        });

        assert_no_diagnostics!(&context);

        let method_ref = context
            .graph()
            .method_references()
            .values()
            .find(|r| *r.str() == StringId::from("||"))
            .unwrap();

        let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
        assert_eq!(StringId::from("<Foo>"), *receiver.str());
        assert!(receiver.nesting().is_none());

        let parent_scope = context
            .graph()
            .names()
            .get(&receiver.parent_scope().expect("Should exist"))
            .unwrap();
        assert_eq!(StringId::from("Foo"), *parent_scope.str());
        assert!(parent_scope.nesting().is_none());
        assert!(parent_scope.parent_scope().is_none());
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
        assert_method_references_eq!(&context, ["m1()", "m2()"]);
    }

    #[test]
    fn index_alias_method_ignores_method_nesting() {
        let context = index_source({
            "
            class Foo
              def bar
                alias_method :new_to_s, :to_s
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-5:4", Class, |foo| {
            assert_definition_at!(&context, "3:5-3:34", MethodAlias, |alias_method| {
                assert_eq!(foo.id(), alias_method.lexical_nesting_id().unwrap());
            });
        });
    }

    #[test]
    fn index_alias_ignores_method_nesting() {
        let context = index_source({
            "
            class Foo
              def bar
                alias new_to_s to_s
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-5:4", Class, |foo| {
            assert_definition_at!(&context, "3:5-3:24", MethodAlias, |alias_method| {
                assert_eq!(foo.id(), alias_method.lexical_nesting_id().unwrap());
            });
        });
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
            assert_def_superclass_ref_eq!(&context, def, "Bar");
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
            assert_def_superclass_ref_eq!(&context, def, "Baz");
            assert_def_name_offset_eq!(&context, def, "1:7-1:10");
        });
    }

    #[test]
    fn ignored_super_classes() {
        let context = index_source({
            "
            class Foo < method_call; end
            class Bar < 123; end
            class MyMigration < ActiveRecord::Migration[8.0]; end
            class Baz < foo::Bar; end
            "
        });

        assert_diagnostics_eq!(
            &context,
            [
                "dynamic-ancestor: Dynamic superclass (1:13-1:24)",
                "dynamic-ancestor: Dynamic superclass (2:13-2:16)",
                "dynamic-ancestor: Dynamic superclass (3:21-3:49)",
                "dynamic-constant-reference: Dynamic constant reference (4:13-4:16)",
                "dynamic-ancestor: Dynamic superclass (4:13-4:21)",
            ]
        );

        assert_definition_at!(&context, "1:1-1:29", Class, |def| {
            assert!(def.superclass_ref().is_none(),);
        });

        assert_definition_at!(&context, "2:1-2:21", Class, |def| {
            assert!(def.superclass_ref().is_none(),);
        });

        assert_definition_at!(&context, "3:1-3:54", Class, |def| {
            assert!(def.superclass_ref().is_none(),);
        });

        assert_definition_at!(&context, "4:1-4:26", Class, |def| {
            assert!(def.superclass_ref().is_none(),);
        });
    }

    #[test]
    fn index_includes_at_top_level() {
        let context = index_source({
            "
            include Bar, Baz
            include Qux
            "
        });

        assert_no_diagnostics!(&context);

        // FIXME: This should be indexed
        assert_eq!(context.graph().definitions().len(), 0);
    }

    #[test]
    fn index_includes_in_classes() {
        let context = index_source({
            "
            class Foo
              include Bar, Baz
              include Qux
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-4:4", Class, |def| {
            assert_def_mixins_eq!(&context, def, Include, ["Baz", "Bar", "Qux"]);
        });
    }

    #[test]
    fn index_includes_in_modules() {
        let context = index_source({
            "
            module Foo
              include Bar, Baz
              include Qux
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-4:4", Module, |def| {
            assert_def_mixins_eq!(&context, def, Include, ["Baz", "Bar", "Qux"]);
        });
    }

    #[test]
    fn index_prepends_at_top_level() {
        let context = index_source({
            "
            prepend Bar, Baz
            prepend Qux
            "
        });

        assert_no_diagnostics!(&context);

        // FIXME: This should be indexed
        assert_eq!(context.graph().definitions().len(), 0);
    }

    #[test]
    fn index_prepends_in_classes() {
        let context = index_source({
            "
            class Foo
              prepend Bar, Baz
              prepend Qux
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-4:4", Class, |def| {
            assert_def_mixins_eq!(&context, def, Prepend, ["Baz", "Bar", "Qux"]);
        });
    }

    #[test]
    fn index_prepends_in_modules() {
        let context = index_source({
            "
            module Foo
              prepend Bar, Baz
              prepend Qux
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-4:4", Module, |def| {
            assert_def_mixins_eq!(&context, def, Prepend, ["Baz", "Bar", "Qux"]);
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
            assert_def_mixins_eq!(&context, class_def, Extend, ["Bar", "Baz"]);
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
            assert_def_mixins_eq!(&context, def, Include, ["Foo"]);
            assert_def_mixins_eq!(&context, def, Prepend, ["Foo"]);
            assert_def_mixins_eq!(&context, def, Extend, ["Foo"]);
        });
    }

    #[test]
    fn index_mixins_with_dynamic_constants() {
        let context = index_source({
            "
            include foo::Bar
            prepend foo::Baz
            extend foo::Qux

            include foo
            prepend 123
            extend 'x'
            "
        });

        assert_diagnostics_eq!(
            &context,
            [
                "dynamic-constant-reference: Dynamic constant reference (1:9-1:12)",
                "dynamic-ancestor: Dynamic mixin argument (1:9-1:17)",
                "dynamic-constant-reference: Dynamic constant reference (2:9-2:12)",
                "dynamic-ancestor: Dynamic mixin argument (2:9-2:17)",
                "dynamic-constant-reference: Dynamic constant reference (3:8-3:11)",
                "dynamic-ancestor: Dynamic mixin argument (3:8-3:16)",
                "dynamic-ancestor: Dynamic mixin argument (5:9-5:12)",
                "dynamic-ancestor: Dynamic mixin argument (6:9-6:12)",
                "dynamic-ancestor: Dynamic mixin argument (7:8-7:11)"
            ]
        );
        assert!(context.graph().definitions().is_empty());
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

        assert_diagnostics_eq!(
            &context,
            [
                "top-level-mixin-self: Top level mixin self (1:9-1:13)",
                "top-level-mixin-self: Top level mixin self (2:9-2:13)",
                "top-level-mixin-self: Top level mixin self (3:8-3:12)"
            ]
        );

        assert_eq!(context.graph().definitions().len(), 0);
    }

    #[test]
    fn index_class_instance_variable() {
        let context = index_source({
            "
            class Foo
              @foo = 0

              class << self
                @bar = 1
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-7:4", Class, |foo_class_def| {
            assert_definition_at!(&context, "2:3-2:7", InstanceVariable, |foo_var_def| {
                assert_def_str_eq!(&context, foo_var_def, "@foo");
                assert_eq!(foo_class_def.id(), foo_var_def.lexical_nesting_id().unwrap());
            });

            assert_definition_at!(&context, "4:3-6:6", SingletonClass, |foo_singleton_def| {
                assert_definition_at!(&context, "5:5-5:9", InstanceVariable, |bar_var_def| {
                    assert_def_str_eq!(&context, bar_var_def, "@bar");
                    assert_eq!(foo_singleton_def.id(), bar_var_def.lexical_nesting_id().unwrap());
                });
            });
        });
    }

    #[test]
    fn index_instance_variable_inside_methods_stay_instance_variable() {
        let context = index_source({
            "
            class Foo
              def initialize
                @bar = 1
              end

              def self.class_method
                @baz = 2
              end

              class << self
                def singleton_method
                  @qux = 3
                end
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "3:5-3:9", InstanceVariable, |def| {
            assert_def_str_eq!(&context, def, "@bar");
        });

        assert_definition_at!(&context, "7:5-7:9", InstanceVariable, |def| {
            assert_def_str_eq!(&context, def, "@baz");
        });

        assert_definition_at!(&context, "12:7-12:11", InstanceVariable, |def| {
            assert_def_str_eq!(&context, def, "@qux");
        });
    }

    #[test]
    fn index_instance_variable_in_method_with_non_self_receiver() {
        let context = index_source({
            "
            class Foo
              def String.bar
                @var = 123
              end
            end
            "
        });

        assert_no_diagnostics!(&context);

        // The instance variable is associated with the singleton class of String.
        // During indexing, we can't know what String resolves to because we haven't
        // resolved constants yet. The lexical nesting is the method definition.
        assert_definition_at!(&context, "1:1-5:4", Class, |_foo_class_def| {
            assert_definition_at!(&context, "2:3-4:6", Method, |method_def| {
                assert_definition_at!(&context, "3:5-3:9", InstanceVariable, |var_def| {
                    assert_def_str_eq!(&context, var_def, "@var");
                    // The lexical nesting of the ivar is the method
                    assert_eq!(method_def.id(), var_def.lexical_nesting_id().unwrap());
                });
            });
        });
    }

    #[test]
    fn index_alias_methods_nested() {
        let context = index_source({
            "
            class Foo
              alias foo bar
              alias :baz :qux
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-4:4", Class, |foo_class_def| {
            assert_definition_at!(&context, "2:3-2:16", MethodAlias, |def| {
                let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
                let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
                assert_eq!(new_name.as_str(), "foo()");
                assert_eq!(old_name.as_str(), "bar()");

                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
            });

            assert_definition_at!(&context, "3:3-3:18", MethodAlias, |def| {
                let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
                let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
                assert_eq!(new_name.as_str(), "baz()");
                assert_eq!(old_name.as_str(), "qux()");

                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
            });
        });
    }

    #[test]
    fn index_alias_methods_top_level() {
        let context = index_source({
            "
            alias foo bar
            alias :baz :qux
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:14", MethodAlias, |def| {
            let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
            let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
            assert_eq!(new_name.as_str(), "foo()");
            assert_eq!(old_name.as_str(), "bar()");

            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:1-2:16", MethodAlias, |def| {
            let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
            let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
            assert_eq!(new_name.as_str(), "baz()");
            assert_eq!(old_name.as_str(), "qux()");

            assert!(def.lexical_nesting_id().is_none());
        });
    }

    #[test]
    fn index_module_alias_method() {
        let context = index_source({
            r#"
            alias_method :foo_symbol, :bar_symbol
            alias_method "foo_string", "bar_string"

            class Foo
              alias_method :baz, :qux
            end

            alias_method :baz, ignored
            alias_method ignored, :qux
            alias_method ignored, ignored
            "#
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:38", MethodAlias, |def| {
            let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
            let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
            assert_eq!(new_name.as_str(), "foo_symbol()");
            assert_eq!(old_name.as_str(), "bar_symbol()");

            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:1-2:40", MethodAlias, |def| {
            let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
            let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
            assert_eq!(new_name.as_str(), "foo_string()");
            assert_eq!(old_name.as_str(), "bar_string()");

            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:1-6:4", Class, |foo_class_def| {
            assert_definition_at!(&context, "5:3-5:26", MethodAlias, |def| {
                let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
                let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
                assert_eq!(new_name.as_str(), "baz()");
                assert_eq!(old_name.as_str(), "qux()");

                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
            });
        });
    }

    #[test]
    fn index_alias_global_variables() {
        let context = index_source({
            "
            alias $foo $bar

            class Foo
              alias $baz $qux
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:16", GlobalVariableAlias, |def| {
            let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
            let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
            assert_eq!(new_name.as_str(), "$foo");
            assert_eq!(old_name.as_str(), "$bar");

            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "3:1-5:4", Class, |foo_class_def| {
            assert_definition_at!(&context, "4:3-4:18", GlobalVariableAlias, |def| {
                let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
                let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
                assert_eq!(new_name.as_str(), "$baz");
                assert_eq!(old_name.as_str(), "$qux");

                assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
            });
        });
    }

    #[test]
    fn index_module_new() {
        let context = index_source({
            "
            module Foo
              Bar = Module.new do
                include Baz

                def qux
                  @var = 123
                end
                attr_reader :hello
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-10:4", Module, |foo| {
            // The constant assignment is at 2:3-2:6 (Bar)
            assert_definition_at!(&context, "2:3-2:6", Constant, |const_def| {
                // The DSL call is at 2:9-9:6 (Module.new do ... end)
                assert_definition_at!(&context, "2:9-9:6", Dsl, |dsl| {
                    // Check Dsl links to Constant via assigned_to
                    assert_eq!(Some(const_def.id()), dsl.assigned_to());
                    assert_eq!(foo.id(), dsl.lexical_nesting_id().unwrap());

                    // Check the include mixin is on the Dsl
                    assert_def_mixins_eq!(&context, dsl, Include, vec!["Baz"]);

                    assert_definition_at!(&context, "5:5-7:8", Method, |qux| {
                        assert_definition_at!(&context, "6:7-6:11", InstanceVariable, |var| {
                            assert_definition_at!(&context, "8:18-8:23", AttrReader, |hello| {
                                // Members are on the Dsl definition
                                assert_eq!(dsl.members()[0], qux.id());
                                assert_eq!(dsl.members()[1], var.id());
                                assert_eq!(dsl.members()[2], hello.id());
                            });
                        });
                    });
                });
            });
        });
    }

    #[test]
    fn index_module_new_with_constant_path() {
        let context = index_source({
            "
            module Foo
              Zip::Bar = Module.new do
                include Baz

                def qux
                  @var = 123
                end
                attr_reader :hello
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-10:4", Module, |foo| {
            // The constant assignment is at 2:8-2:11 (Bar in Zip::Bar)
            assert_definition_at!(&context, "2:8-2:11", Constant, |const_def| {
                // The DSL call is at 2:14-9:6 (Module.new do ... end)
                assert_definition_at!(&context, "2:14-9:6", Dsl, |dsl| {
                    // Check Dsl links to Constant via assigned_to
                    assert_eq!(Some(const_def.id()), dsl.assigned_to());
                    assert_eq!(foo.id(), dsl.lexical_nesting_id().unwrap());

                    // Check the include mixin is on the Dsl
                    assert_def_mixins_eq!(&context, dsl, Include, vec!["Baz"]);

                    assert_definition_at!(&context, "5:5-7:8", Method, |qux| {
                        assert_definition_at!(&context, "6:7-6:11", InstanceVariable, |var| {
                            assert_definition_at!(&context, "8:18-8:23", AttrReader, |hello| {
                                // Members are on the Dsl definition
                                assert_eq!(dsl.members()[0], qux.id());
                                assert_eq!(dsl.members()[1], var.id());
                                assert_eq!(dsl.members()[2], hello.id());
                            });
                        });
                    });
                });
            });
        });
    }

    #[test]
    fn index_class_new() {
        let context = index_source({
            "
            module Foo
              Bar = Class.new(Parent) do
                include Baz

                def qux
                  @var = 123
                end
                attr_reader :hello
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-10:4", Module, |foo| {
            assert_definition_at!(&context, "2:3-2:6", Constant, |const_def| {
                assert_definition_at!(&context, "2:9-9:6", Dsl, |dsl| {
                    assert_eq!(Some(const_def.id()), dsl.assigned_to());
                    assert_eq!(foo.id(), dsl.lexical_nesting_id().unwrap());
                    // Class.new(Parent) - Parent is indexed as a constant reference
                    assert!(dsl.arguments().first_positional_reference().is_some());
                    assert_block_offset_eq!(&context, "2:27-9:6", dsl.arguments());
                    assert_def_mixins_eq!(&context, dsl, Include, vec!["Baz"]);

                    assert_definition_at!(&context, "5:5-7:8", Method, |qux| {
                        assert_definition_at!(&context, "6:7-6:11", InstanceVariable, |var| {
                            assert_definition_at!(&context, "8:18-8:23", AttrReader, |hello| {
                                assert_eq!(dsl.members()[0], qux.id());
                                assert_eq!(dsl.members()[1], var.id());
                                assert_eq!(dsl.members()[2], hello.id());
                            });
                        });
                    });
                });
            });
        });
    }

    #[test]
    fn index_class_new_no_parent() {
        let context = index_source({
            "
            module Foo
              Bar = Class.new do
                include Baz

                def qux
                  @var = 123
                end
                attr_reader :hello
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-10:4", Module, |foo| {
            assert_definition_at!(&context, "2:3-2:6", Constant, |const_def| {
                assert_definition_at!(&context, "2:9-9:6", Dsl, |dsl| {
                    assert_eq!(Some(const_def.id()), dsl.assigned_to());
                    assert_eq!(foo.id(), dsl.lexical_nesting_id().unwrap());
                    assert_block_offset_eq!(&context, "2:19-9:6", dsl.arguments());
                    assert_def_mixins_eq!(&context, dsl, Include, vec!["Baz"]);

                    assert_definition_at!(&context, "5:5-7:8", Method, |qux| {
                        assert_definition_at!(&context, "6:7-6:11", InstanceVariable, |var| {
                            assert_definition_at!(&context, "8:18-8:23", AttrReader, |hello| {
                                assert_eq!(dsl.members()[0], qux.id());
                                assert_eq!(dsl.members()[1], var.id());
                                assert_eq!(dsl.members()[2], hello.id());
                            });
                        });
                    });
                });
            });
        });
    }

    #[test]
    fn index_class_new_with_constant_path() {
        let context = index_source({
            "
            module Foo
              Zip::Bar = Class.new(Parent) do
                include Baz

                def qux
                  @var = 123
                end
                attr_reader :hello
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-10:4", Module, |foo| {
            assert_definition_at!(&context, "2:8-2:11", Constant, |const_def| {
                assert_definition_at!(&context, "2:14-9:6", Dsl, |dsl| {
                    assert_eq!(Some(const_def.id()), dsl.assigned_to());
                    assert_eq!(foo.id(), dsl.lexical_nesting_id().unwrap());
                    // Class.new(Parent) - Parent is indexed as a constant reference
                    assert!(dsl.arguments().first_positional_reference().is_some());
                    assert_block_offset_eq!(&context, "2:32-9:6", dsl.arguments());
                    assert_def_mixins_eq!(&context, dsl, Include, vec!["Baz"]);

                    assert_definition_at!(&context, "5:5-7:8", Method, |qux| {
                        assert_definition_at!(&context, "6:7-6:11", InstanceVariable, |var| {
                            assert_definition_at!(&context, "8:18-8:23", AttrReader, |hello| {
                                assert_eq!(dsl.members()[0], qux.id());
                                assert_eq!(dsl.members()[1], var.id());
                                assert_eq!(dsl.members()[2], hello.id());
                            });
                        });
                    });
                });
            });
        });
    }

    #[test]
    fn index_top_level_class_and_module_new() {
        let context = index_source({
            "
            module Foo
              Bar = ::Class.new do
              end

              Baz = ::Module.new do
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-7:4", Module, |foo| {
            // Bar = ::Class.new do end
            assert_definition_at!(&context, "2:9-3:6", Dsl, |bar_dsl| {
                assert_definition_at!(&context, "2:3-2:6", Constant, |bar_const| {
                    // Baz = ::Module.new do end
                    assert_definition_at!(&context, "5:9-6:6", Dsl, |baz_dsl| {
                        assert_definition_at!(&context, "5:3-5:6", Constant, |baz_const| {
                            // Check Dsl links to Constant via assigned_to
                            assert_eq!(Some(bar_const.id()), bar_dsl.assigned_to());
                            assert_eq!(Some(baz_const.id()), baz_dsl.assigned_to());

                            // Check lexical nesting - Dsl definitions are nested in Foo
                            assert_eq!(foo.id(), bar_dsl.lexical_nesting_id().unwrap());
                            assert_eq!(foo.id(), baz_dsl.lexical_nesting_id().unwrap());
                        });
                    });
                });
            });
        });
    }

    #[test]
    fn index_anonymous_class_and_module_new() {
        let context = index_source({
            "
            module Foo
              Class.new do
                def bar; end
              end

              Module.new do
                def baz; end
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-9:4", Module, |foo| {
            assert_definition_at!(&context, "2:3-4:6", Dsl, |dsl| {
                assert_eq!(foo.id(), dsl.lexical_nesting_id().unwrap());
                assert_block_offset_eq!(&context, "2:13-4:6", dsl.arguments());

                assert_definition_at!(&context, "3:5-3:17", Method, |bar| {
                    assert_eq!(dsl.id(), bar.lexical_nesting_id().unwrap());
                });
            });

            assert_definition_at!(&context, "6:3-8:6", Dsl, |dsl| {
                assert_eq!(foo.id(), dsl.lexical_nesting_id().unwrap());
                assert_block_offset_eq!(&context, "6:14-8:6", dsl.arguments());

                assert_definition_at!(&context, "7:5-7:17", Method, |baz| {
                    assert_eq!(dsl.id(), baz.lexical_nesting_id().unwrap());
                });
            });
        });
    }

    #[test]
    fn index_nested_class_and_module_new() {
        let context = index_source({
            "
            module Foo
              Class.new do
                Module.new do
                end
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-6:4", Module, |foo| {
            assert_definition_at!(&context, "2:3-5:6", Dsl, |outer_dsl| {
                assert_eq!(foo.id(), outer_dsl.lexical_nesting_id().unwrap());
                assert_block_offset_eq!(&context, "2:13-5:6", outer_dsl.arguments());

                assert_definition_at!(&context, "3:5-4:8", Dsl, |inner_dsl| {
                    assert_eq!(foo.id(), inner_dsl.lexical_nesting_id().unwrap());
                    assert_block_offset_eq!(&context, "3:16-4:8", inner_dsl.arguments());
                });
            });
        });
    }

    #[test]
    fn index_named_module_nested_inside_anonymous() {
        let context = index_source({
            "
            module Foo
              Class.new do
                module Bar
                end
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-6:4", Module, |foo| {
            assert_definition_at!(&context, "2:3-5:6", Dsl, |dsl| {
                assert_eq!(foo.id(), dsl.lexical_nesting_id().unwrap());
                assert_block_offset_eq!(&context, "2:13-5:6", dsl.arguments());

                assert_definition_at!(&context, "3:5-4:8", Module, |bar| {
                    assert_eq!(foo.id(), bar.lexical_nesting_id().unwrap());
                });
            });
        });
    }

    #[test]
    fn index_anonymous_namespace_mixins() {
        let context = index_source({
            "
            module Foo
              Class.new do
                include Bar
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-5:4", Module, |foo| {
            assert_definition_at!(&context, "2:3-4:6", Dsl, |dsl| {
                assert_eq!(foo.id(), dsl.lexical_nesting_id().unwrap());
                assert_block_offset_eq!(&context, "2:13-4:6", dsl.arguments());
                assert_def_mixins_eq!(&context, dsl, Include, vec!["Bar"]);
            });
        });
    }

    #[test]
    fn index_singleton_method_in_class_new() {
        let context = index_source({
            "
            module Foo
              A = Class.new do
                def self.bar
                end
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "2:7-5:6", Dsl, |dsl| {
            assert_definition_at!(&context, "3:5-4:8", Method, |bar| {
                // def self.bar - receiver is the DSL definition
                let receiver = bar.receiver().unwrap();
                match receiver {
                    Receiver::SelfReceiver(def_id) => {
                        assert_eq!(dsl.id(), def_id);
                    }
                    Receiver::ConstantReceiver(_) => panic!("expected SelfReceiver"),
                }

                // lexical_nesting_id should point to the DSL definition
                assert_eq!(Some(dsl.id()), *bar.lexical_nesting_id());
            });
        });
    }

    #[test]
    fn index_class_variable_in_class_new() {
        let context = index_source({
            "
            module Foo
              A = Class.new do
                def bar
                  @@var = 123
                end
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-7:4", Module, |foo| {
            assert_definition_at!(&context, "2:7-6:6", Dsl, |dsl| {
                assert_eq!(foo.id(), dsl.lexical_nesting_id().unwrap());
                assert_definition_at!(&context, "4:7-4:12", ClassVariable, |var| {
                    assert_eq!(foo.id(), var.lexical_nesting_id().unwrap());
                });
            });
        });
    }

    #[test]
    fn index_class_new_with_path_assignment() {
        let context = index_source({
            "
            module Foo; end
            Foo::Bar = Class.new
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "2:6-2:9", Constant, |bar| {
            assert_definition_at!(&context, "2:12-2:21", Dsl, |dsl| {
                assert_eq!(bar.id(), dsl.assigned_to().unwrap());
            });
        });
    }

    #[test]
    fn index_class_new_with_or_assignment() {
        let context = index_source({
            "
            Foo ||= Class.new
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:4", Constant, |foo| {
            assert_definition_at!(&context, "1:9-1:18", Dsl, |dsl| {
                assert_eq!(foo.id(), dsl.assigned_to().unwrap());
                assert_constant_references_eq!(&context, ["Foo", "Class"]);
            });
        });
    }

    #[test]
    fn index_class_new_with_path_or_assignment() {
        let context = index_source({
            "
            module Foo; end
            Foo::Bar ||= Class.new
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "2:6-2:9", Constant, |bar| {
            assert_definition_at!(&context, "2:14-2:23", Dsl, |dsl| {
                assert_eq!(bar.id(), dsl.assigned_to().unwrap());
                assert_constant_references_eq!(&context, ["Foo", "Bar", "Class"]);
            });
        });
    }

    #[test]
    fn index_class_new_with_chained_assignment() {
        // For `A = B = Class.new`, Ruby evaluates right-to-left:
        // - Class.new creates a class
        // - Assigned to B first (B is the primary constant)
        // - Then assigned to A (A is an alias to B)
        let context = index_source({
            "
            A = B = Class.new
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-1:2", ConstantAlias, |a| {
            assert_definition_at!(&context, "1:5-1:6", Constant, |b| {
                assert_definition_at!(&context, "1:9-1:18", Dsl, |dsl| {
                    assert_eq!(b.id(), dsl.assigned_to().unwrap());
                    assert_eq!(a.target_name_id(), b.name_id());
                });
            });
        });
    }

    #[test]
    fn index_alias_method_in_anonymous_class() {
        let context = index_source({
            "
            Class.new do
              alias_method :bar, :baz
            end
            "
        });
        assert_no_diagnostics!(&context);
        assert_method_references_eq!(&context, ["baz()"]);
    }

    #[test]
    fn index_method_reference_inside_anonymous_class() {
        let context = index_source({
            "
            Class.new do
              def bar
                baz
              end
            end
            "
        });
        assert_no_diagnostics!(&context);
        assert_method_references_eq!(&context, ["baz"]);
    }

    #[test]
    fn index_singleton_method_in_anonymous_namespace() {
        let context = index_source({
            "
            module Foo
              Class.new do
                def self.bar
                end
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "2:3-5:6", Dsl, |dsl| {
            assert_definition_at!(&context, "3:5-4:8", Method, |bar| {
                // def self.bar - receiver is the DSL definition
                let receiver = bar.receiver().unwrap();
                match receiver {
                    Receiver::SelfReceiver(def_id) => {
                        assert_eq!(dsl.id(), def_id);
                    }
                    Receiver::ConstantReceiver(_) => panic!("expected SelfReceiver"),
                }

                // lexical_nesting_id should point to the DSL definition
                assert_eq!(Some(dsl.id()), *bar.lexical_nesting_id());
            });
        });
    }

    #[test]
    fn index_singleton_class_inside_class_new() {
        let context = index_source({
            "
            Foo = Class.new do
              class << self
                def singleton_method; end
              end
            end
            "
        });

        assert_definition_at!(&context, "1:7-5:4", Dsl, |dsl| {
            assert_definition_at!(&context, "2:3-4:6", SingletonClass, |singleton| {
                // Singleton class is nested in the DSL
                assert_eq!(singleton.lexical_nesting_id(), &Some(dsl.id()));

                // Method inside singleton
                assert_definition_at!(&context, "3:5-3:30", Method, |method| {
                    assert_eq!(method.lexical_nesting_id(), &Some(singleton.id()));
                });
            });
        });
    }

    #[test]
    fn index_nested_class_new_inside_singleton_class() {
        let context = index_source({
            "
            Foo = Class.new do
              class << self
                Bar = Class.new do
                  def bar_method; end
                end
              end
            end
            "
        });

        assert_definition_at!(&context, "2:3-6:6", SingletonClass, |singleton| {
            assert_definition_at!(&context, "3:5-3:8", Constant, |bar_const| {
                assert_definition_at!(&context, "3:11-5:8", Dsl, |inner_dsl| {
                    assert_eq!(Some(bar_const.id()), inner_dsl.assigned_to());
                    assert_eq!(inner_dsl.lexical_nesting_id(), &Some(singleton.id()));
                });
            });
        });
    }

    #[test]
    fn index_nested_method_definitions() {
        let context = index_source({
            "
            class Foo
              def bar
                def baz; end
              end
            end
            "
        });
        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-5:4", Class, |foo| {
            assert_definition_at!(&context, "2:3-4:6", Method, |bar| {
                assert_definition_at!(&context, "3:5-3:17", Method, |baz| {
                    assert_eq!(foo.id(), bar.lexical_nesting_id().unwrap());
                    assert_eq!(foo.id(), baz.lexical_nesting_id().unwrap());
                });
            });
        });
    }

    #[test]
    fn index_constant_alias_simple() {
        let context = index_source({
            "
            module Foo; end
            ALIAS1 = Foo
            ALIAS2 ||= Foo
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "2:1-2:7", ConstantAlias, |def| {
            assert_def_name_eq!(&context, def, "ALIAS1");
            assert_name_path_eq!(&context, "Foo", *def.target_name_id());
        });
        assert_definition_at!(&context, "3:1-3:7", ConstantAlias, |def| {
            assert_def_name_eq!(&context, def, "ALIAS2");
            assert_name_path_eq!(&context, "Foo", *def.target_name_id());
        });
    }

    #[test]
    fn index_constant_alias_to_path() {
        let context = index_source({
            "
            module Foo
              module Bar; end
            end
            ALIAS = Foo::Bar
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "4:1-4:6", ConstantAlias, |def| {
            assert_def_name_eq!(&context, def, "ALIAS");
            assert_name_path_eq!(&context, "Foo::Bar", *def.target_name_id());
        });

        assert_constant_references_eq!(&context, ["Foo", "Bar"]);
    }

    #[test]
    fn index_constant_alias_nested() {
        let context = index_source({
            "
            module Foo; end
            module Bar
              MyFoo = Foo
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "2:1-4:4", Module, |bar_module_def| {
            assert_definition_at!(&context, "3:3-3:8", ConstantAlias, |def| {
                assert_def_name_eq!(&context, def, "MyFoo");
                assert_eq!(bar_module_def.id(), def.lexical_nesting_id().unwrap());
            });
        });
    }

    #[test]
    fn index_scoped_constant_alias() {
        let context = index_source({
            "
            module Foo; end
            module Bar; end
            Bar::ALIAS = Foo
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "3:6-3:11", ConstantAlias, |def| {
            assert_def_name_eq!(&context, def, "Bar::ALIAS");
        });
    }

    #[test]
    fn index_chained_constant_alias() {
        let context = index_source({
            "
            module Target; end
            A = B = Target
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "2:1-2:2", ConstantAlias, |def| {
            assert_def_name_eq!(&context, def, "A");
            assert_name_path_eq!(&context, "Target", *def.target_name_id());
        });
        assert_definition_at!(&context, "2:5-2:6", ConstantAlias, |def| {
            assert_def_name_eq!(&context, def, "B");
            assert_name_path_eq!(&context, "Target", *def.target_name_id());
        });

        assert_constant_references_eq!(&context, ["Target"]);
    }

    #[test]
    fn index_constant_alias_to_top_level_constant() {
        let context = index_source({
            "
            module Foo; end
            ALIAS = ::Foo
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "2:1-2:6", ConstantAlias, |def| {
            assert_def_name_eq!(&context, def, "ALIAS");
            assert_name_path_eq!(&context, "Foo", *def.target_name_id());
        });
    }

    #[test]
    fn index_constant_alias_chain() {
        let context = index_source({
            "
            module Foo; end
            ALIAS1 = Foo
            ALIAS2 = ALIAS1
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "2:1-2:7", ConstantAlias, |def| {
            assert_def_name_eq!(&context, def, "ALIAS1");
            assert_name_path_eq!(&context, "Foo", *def.target_name_id());
        });
        assert_definition_at!(&context, "3:1-3:7", ConstantAlias, |def| {
            assert_def_name_eq!(&context, def, "ALIAS2");
            assert_name_path_eq!(&context, "ALIAS1", *def.target_name_id());
        });
    }

    // Comments

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
            assert_def_name_eq!(&context, def, "Single");
            assert_def_comments_eq!(&context, def, ["# Single comment"]);
        });

        assert_definition_at!(&context, "7:1-7:18", Module, |def| {
            assert_def_name_eq!(&context, def, "Multi");
            assert_def_comments_eq!(
                &context,
                def,
                [
                    "# Multi-line comment 1",
                    "# Multi-line comment 2",
                    "# Multi-line comment 3"
                ]
            );
        });

        assert_definition_at!(&context, "12:1-12:28", Class, |def| {
            assert_def_name_eq!(&context, def, "EmptyCommentLine");
            assert_def_comments_eq!(&context, def, ["# Comment 1", "#", "# Comment 2"]);
        });

        assert_definition_at!(&context, "15:1-15:6", Constant, |def| {
            assert_def_name_eq!(&context, def, "NoGap");
            assert_def_comments_eq!(&context, def, ["# Comment directly above (no gap)"]);
        });

        assert_definition_at!(&context, "19:1-19:13", Method, |def| {
            assert_def_str_eq!(&context, def, "foo()");
            assert_def_comments_eq!(&context, def, ["#: ()", "#| -> void"]);
        });

        assert_definition_at!(&context, "23:1-23:21", Class, |def| {
            assert_def_name_eq!(&context, def, "BlankLine");
            assert_def_comments_eq!(&context, def, ["# Comment with blank line"]);
        });

        assert_definition_at!(&context, "28:1-28:21", Class, |def| {
            assert_def_name_eq!(&context, def, "NoComment");
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
            assert_def_name_eq!(&context, def, "Outer");
            assert_def_comments_eq!(&context, def, ["# Outer class"]);
        });

        assert_definition_at!(&context, "4:3-7:6", Class, |def| {
            assert_def_name_eq!(&context, def, "Inner");
            assert_def_comments_eq!(&context, def, ["# Inner class at 2 spaces"]);
        });

        assert_definition_at!(&context, "6:5-6:20", Class, |def| {
            assert_def_name_eq!(&context, def, "Deep");
            assert_def_comments_eq!(&context, def, ["# Deep class at 4 spaces"]);
        });

        assert_definition_at!(&context, "11:3-11:26", Class, |def| {
            assert_def_name_eq!(&context, def, "AnotherInner");
            assert_def_comments_eq!(&context, def, ["# Another inner class", "# with multiple lines"]);
        });
    }

    #[test]
    fn index_comments_with_tags() {
        let context = index_source({
            "
            # @deprecated
            class Deprecated; end

            class NotDeprecated; end

            # Multi-line comment
            # @deprecated Use something else
            def deprecated_method; end

            # Not @deprecated
            def not_deprecated_method; end
            "
        });

        assert!(context.definition_at("2:1-2:22").is_deprecated());
        assert!(!context.definition_at("4:1-4:25").is_deprecated());
        assert!(context.definition_at("8:1-8:27").is_deprecated());
        assert!(!context.definition_at("11:1-11:31").is_deprecated());
    }

    #[test]
    fn index_comments_attr_accessor() {
        let context = index_source({
            "
            class Foo
              # Comment
              attr_reader :foo

              # Comment 1
              # Comment 2
              # Comment 3
              attr_writer :bar

              # Comment 1
              # Comment 2
              # Comment 3
              attr_accessor :baz, :qux

              # Comment
              attr :quux, true
            end
            "
        });

        assert_no_diagnostics!(&context);

        assert_definition_at!(&context, "3:16-3:19", AttrReader, |def| {
            assert_def_comments_eq!(&context, def, ["# Comment"]);
        });

        assert_definition_at!(&context, "8:16-8:19", AttrWriter, |def| {
            assert_def_comments_eq!(&context, def, ["# Comment 1", "# Comment 2", "# Comment 3"]);
        });

        assert_definition_at!(&context, "13:18-13:21", AttrAccessor, |def| {
            assert_def_comments_eq!(&context, def, ["# Comment 1", "# Comment 2", "# Comment 3"]);
        });

        assert_definition_at!(&context, "13:24-13:27", AttrAccessor, |def| {
            assert_def_comments_eq!(&context, def, ["# Comment 1", "# Comment 2", "# Comment 3"]);
        });

        assert_definition_at!(&context, "16:9-16:13", AttrAccessor, |def| {
            assert_def_comments_eq!(&context, def, ["# Comment"]);
        });
    }

    #[test]
    fn index_comments_visibility() {
        let context = index_source({
            "
            class Foo
              # Comment
              private def foo; end

              # Comment
              protected def bar; end

              # Comment
              public def baz; end

              # Comment
              private attr_reader :qux
            end
            "
        });

        assert_definition_at!(&context, "3:11-3:23", Method, |def| {
            assert_def_comments_eq!(&context, def, ["# Comment"]);
        });

        assert_definition_at!(&context, "6:13-6:25", Method, |def| {
            assert_def_comments_eq!(&context, def, ["# Comment"]);
        });

        assert_definition_at!(&context, "9:10-9:22", Method, |def| {
            assert_def_comments_eq!(&context, def, ["# Comment"]);
        });

        assert_definition_at!(&context, "12:24-12:27", AttrReader, |def| {
            assert_def_comments_eq!(&context, def, ["# Comment"]);
        });
    }

    #[test]
    fn index_dsl_captures_all_argument_types() {
        let context = index_source({
            r#"
            Class.new(Parent, "string_arg", *splat_args, key: value, **double_splat) do
            end
            "#
        });
        assert_no_diagnostics!(&context);

        let dsl_defs: Vec<_> = context
            .graph()
            .definitions()
            .iter()
            .filter_map(|(_, def)| match def {
                Definition::Dsl(d) => Some(d),
                _ => None,
            })
            .collect();

        assert_eq!(dsl_defs.len(), 1);
        let args = dsl_defs[0].arguments();

        let arg_list = args.arguments();
        assert_eq!(arg_list.len(), 5);

        // First argument is a constant reference (Parent)
        let DslArgument::Positional(v) = &arg_list[0] else {
            panic!("expected Positional")
        };
        let ref_id = v.as_reference().expect("expected constant reference for Parent");
        let ref_entry = context.graph().constant_references().get(&ref_id).unwrap();
        let name = context.graph().names().get(ref_entry.name_id()).unwrap();
        let name_str = context.graph().strings().get(name.str()).unwrap();
        assert_eq!(name_str.as_str(), "Parent");

        // Second argument is a string literal
        let DslArgument::Positional(v) = &arg_list[1] else {
            panic!("expected Positional")
        };
        assert_eq!(v.as_str().unwrap(), "\"string_arg\"");

        let DslArgument::Splat(s) = &arg_list[2] else {
            panic!("expected Splat")
        };
        assert_eq!(s, "splat_args");

        let DslArgument::KeywordArg { key, value } = &arg_list[3] else {
            panic!("expected KeywordArg")
        };
        assert_eq!(key, "key");
        assert_eq!(value.as_str().unwrap(), "value");

        let DslArgument::DoubleSplat(s) = &arg_list[4] else {
            panic!("expected DoubleSplat")
        };
        assert_eq!(s, "double_splat");

        assert_block_offset_eq!(&context, "1:74-2:4", args);
    }

    #[test]
    fn index_dsl_captures_block_argument() {
        // &proc is treated as a block by Prism, not as an argument
        let context = index_source({
            "
            proc = -> { }
            Class.new(&proc) # Class.new is captured as DSL
            "
        });
        assert_no_diagnostics!(&context);

        let dsl_defs: Vec<_> = context
            .graph()
            .definitions()
            .iter()
            .filter_map(|(_, def)| match def {
                Definition::Dsl(d) => Some(d),
                _ => None,
            })
            .collect();

        assert_eq!(dsl_defs.len(), 1);
        let args = dsl_defs[0].arguments();

        assert!(args.arguments().is_empty());
        assert_block_offset_eq!(&context, "2:11-2:16", args);
    }
}
