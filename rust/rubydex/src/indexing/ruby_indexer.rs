//! Visit the Ruby AST and create the definitions.

use crate::diagnostic::Rule;
use crate::indexing::local_graph::LocalGraph;
use crate::model::comment::Comment;
use crate::model::definitions::{
    AttrAccessorDefinition, AttrReaderDefinition, AttrWriterDefinition, ClassDefinition, ClassVariableDefinition,
    ConstantAliasDefinition, ConstantDefinition, ConstantVisibilityDefinition, Definition, DefinitionFlags,
    ExtendDefinition, GlobalVariableAliasDefinition, GlobalVariableDefinition, IncludeDefinition,
    InstanceVariableDefinition, MethodAliasDefinition, MethodDefinition, MethodVisibilityDefinition, Mixin,
    ModuleDefinition, Parameter, ParameterStruct, PrependDefinition, Receiver, Signatures, SingletonClassDefinition,
};
use crate::model::document::Document;
use crate::model::ids::{DefinitionId, NameId, StringId, UriId};
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
    /// An owner entry that will be associated with all members encountered, but will not produce a new lexical scope
    /// (e.g.: Module.new or Class.new)
    Owner(DefinitionId),
    /// A method entry that is used to set the correct owner for instance variables, but cannot own anything itself
    Method(DefinitionId),
}

impl Nesting {
    fn id(&self) -> DefinitionId {
        match self {
            Nesting::LexicalScope(id) | Nesting::Owner(id) | Nesting::Method(id) => *id,
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
    pending_decorator_offset: Option<Offset>,
}

impl<'a> RubyIndexer<'a> {
    #[must_use]
    pub fn new(uri: String, source: &'a str) -> Self {
        let uri_id = UriId::from(&uri);
        let local_graph = LocalGraph::new(uri_id, Document::new(uri, source));

        Self {
            uri_id,
            local_graph,
            source,
            comments: Vec::new(),
            nesting_stack: Vec::new(),
            visibility_stack: vec![VisibilityModifier::new(Visibility::Private, false, Offset::new(0, 0))],
            pending_decorator_offset: None,
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

    fn offset_to_string(&self, offset: &Offset) -> String {
        self.source[offset.start() as usize..offset.end() as usize].to_string()
    }

    fn find_comments_for(&self, offset: u32) -> (Box<[Comment]>, DefinitionFlags) {
        let offset_usize = offset as usize;
        if self.comments.is_empty() {
            return (Box::default(), DefinitionFlags::empty());
        }

        let idx = match self.comments.binary_search_by_key(&offset_usize, |g| g.end_offset) {
            Ok(_) => {
                // This should never happen in valid Ruby syntax - a comment cannot end exactly
                // where a definition begins (there must be at least a newline between them)
                debug_assert!(false, "Comment ends exactly at definition start - this indicates a bug");
                return (Box::default(), DefinitionFlags::empty());
            }
            Err(i) if i > 0 => i - 1,
            Err(_) => return (Box::default(), DefinitionFlags::empty()),
        };

        let group = &self.comments[idx];
        let between = &self.source.as_bytes()[group.end_offset..offset_usize];
        if !between.iter().all(|&b| b.is_ascii_whitespace()) {
            return (Box::default(), DefinitionFlags::empty());
        }

        // We allow at most one blank line between the comment and the definition
        if bytecount::count(between, b'\n') > 2 {
            return (Box::default(), DefinitionFlags::empty());
        }

        (group.comments(), group.flags())
    }

    /// We consider comments above a method decorator like Sorbet's sig to be documentation for methods and attributes.
    /// To find the correct comment offset, we remember the offsets for the sigs we find
    fn take_decorator_offset(&mut self, definition_start: u32) -> Option<u32> {
        let decorator_offset = self.pending_decorator_offset.take()?;
        if decorator_offset.end() > definition_start {
            return None;
        }

        let between = &self.source.as_bytes()[decorator_offset.end() as usize..definition_start as usize];
        if between.iter().all(|&b| b.is_ascii_whitespace()) && bytecount::count(between, b'\n') <= 1 {
            Some(decorator_offset.start())
        } else {
            None
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
                self.visit(&opt_param.value());
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
                        let loc = required.name_loc();
                        let full = Offset::from_prism_location(&loc);
                        let offset = Offset::new(full.start(), full.end() - 1); // Exclude trailing colon
                        let str_id = self.local_graph.intern_string(self.offset_to_string(&offset));

                        parameters.push(Parameter::RequiredKeyword(ParameterStruct::new(offset, str_id)));
                    }
                    ruby_prism::Node::OptionalKeywordParameterNode { .. } => {
                        let optional = keyword.as_optional_keyword_parameter_node().unwrap();
                        let loc = optional.name_loc();
                        let full = Offset::from_prism_location(&loc);
                        let offset = Offset::new(full.start(), full.end() - 1); // Exclude trailing colon
                        let str_id = self.local_graph.intern_string(self.offset_to_string(&offset));

                        parameters.push(Parameter::OptionalKeyword(ParameterStruct::new(offset, str_id)));
                        self.visit(&optional.value());
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
    /// Used to resolve `self` to a concrete `NameId` during indexing.
    ///
    /// Iterates through the definitions stack in reverse to find the first class/module/singleton class, skipping
    /// methods. Ignores `Class.new` and other owners that do not produce lexical scopes
    ///
    /// # Panics
    ///
    /// Panics if the definition is not a class, module, or singleton class
    fn current_lexical_scope_name_id(&self) -> Option<NameId> {
        self.nesting_stack.iter().rev().find_map(|nesting| match nesting {
            Nesting::LexicalScope(id) => {
                if let Some(definition) = self.local_graph.definitions().get(id) {
                    match definition {
                        Definition::Class(class_def) => Some(*class_def.name_id()),
                        Definition::Module(module_def) => Some(*module_def.name_id()),
                        Definition::SingletonClass(singleton_class_def) => Some(*singleton_class_def.name_id()),
                        Definition::Method(_) => None,
                        _ => panic!("current nesting is not a class/module/singleton class: {definition:?}"),
                    }
                } else {
                    None
                }
            }
            Nesting::Method(_) | Nesting::Owner(_) => None,
        })
    }

    /// Gets the `NameId` of the current owner (class/module/singleton class), including `Class.new`/`Module.new`.
    /// Used to resolve `self` in singleton method definitions (e.g., `def self.bar`).
    ///
    /// Unlike `current_lexical_scope_name_id`, this method considers `Nesting::Owner` entries,
    /// because `self` inside a `Class.new` block refers to the new class being created.
    fn current_owner_name_id(&self) -> Option<NameId> {
        self.nesting_stack.iter().rev().find_map(|nesting| match nesting {
            Nesting::LexicalScope(id) | Nesting::Owner(id) => {
                if let Some(definition) = self.local_graph.definitions().get(id) {
                    match definition {
                        Definition::Class(class_def) => Some(*class_def.name_id()),
                        Definition::Module(module_def) => Some(*module_def.name_id()),
                        Definition::SingletonClass(singleton_class_def) => Some(*singleton_class_def.name_id()),
                        Definition::Method(_) => None,
                        _ => panic!("current nesting is not a class/module/singleton class: {definition:?}"),
                    }
                } else {
                    None
                }
            }
            Nesting::Method(_) => None,
        })
    }

    // Runs the given closure for each string or symbol argument of a call node.
    fn each_string_or_symbol_arg<F>(node: &ruby_prism::CallNode, mut f: F)
    where
        F: FnMut(String, ruby_prism::Location),
    {
        if let Some(arguments) = node.arguments() {
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
        F: FnOnce(StringId, Offset, Box<[Comment]>, DefinitionFlags, Option<DefinitionId>, UriId) -> Definition,
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

    /// Returns whether a value node represents a method call that could produce a class or module.
    /// Only regular method calls (bare calls or dot/safe-nav calls) are considered promotable.
    /// Operator calls like `1 + 2` are `CallNode`s in Prism but should not be promotable.
    fn is_promotable_value(value: &ruby_prism::Node) -> bool {
        value.as_call_node().is_some_and(|call| {
            // Bare calls (no receiver): `some_factory_call`
            // Dot/safe-nav/scope calls: `Struct.new(...)`, `foo&.bar`, `Struct::new`
            // Excluded: operator calls like `1 + 2` which have a receiver but no call operator
            call.receiver().is_none() || call.call_operator_loc().is_some()
        })
    }

    fn add_constant_definition(
        &mut self,
        node: &ruby_prism::Node,
        also_add_reference: bool,
        promotable: bool,
    ) -> Option<DefinitionId> {
        let name_id = self.index_constant_reference(node, also_add_reference)?;

        // Get the location for the constant name/path only (not including the value)
        let location = match node {
            ruby_prism::Node::ConstantWriteNode { .. } => node.as_constant_write_node().unwrap().name_loc(),
            ruby_prism::Node::ConstantOrWriteNode { .. } => node.as_constant_or_write_node().unwrap().name_loc(),
            ruby_prism::Node::ConstantPathNode { .. } => node.as_constant_path_node().unwrap().name_loc(),
            _ => node.location(),
        };

        let offset = Offset::from_prism_location(&location);
        let (comments, mut flags) = self.find_comments_for(offset.start());
        if promotable {
            flags |= DefinitionFlags::PROMOTABLE;
        }
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
            // Try direct constant reference first
            if let Some(id) = self.index_constant_reference(n, false) {
                return Some(self.local_graph.add_constant_reference(ConstantReference::new(
                    id,
                    self.uri_id,
                    Offset::from_prism_location(&n.location()),
                )));
            }

            // For call nodes (e.g. `ActiveRecord::Migration[7.0]`), try the receiver constant
            if let ruby_prism::Node::CallNode { .. } = n {
                let call = n.as_call_node().unwrap();
                if let Some(receiver) = call.receiver()
                    && let Some(id) = self.index_constant_reference(&receiver, false)
                {
                    return Some(self.local_graph.add_constant_reference(ConstantReference::new(
                        id,
                        self.uri_id,
                        Offset::from_prism_location(&receiver.location()),
                    )));
                }
            }

            None
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

    /// Handle dynamic class or module definitions, like `Module.new`, `Class.new`, `Data.define` and so on
    fn handle_dynamic_class_or_module(&mut self, node: &ruby_prism::Node, value: &ruby_prism::Node) -> bool {
        let Some(call_node) = value.as_call_node() else {
            return false;
        };

        if call_node.name().as_slice() != b"new" {
            return false;
        }

        let Some(receiver) = call_node.receiver() else {
            return false;
        };

        let receiver_name = receiver.location().as_slice();

        if matches!(receiver_name, b"Module" | b"::Module") {
            self.handle_module_definition(&node.location(), Some(node), call_node.block(), Nesting::Owner);
        } else if matches!(receiver_name, b"Class" | b"::Class") {
            self.handle_class_definition(
                &node.location(),
                Some(node),
                call_node.block(),
                call_node.arguments().and_then(|args| args.arguments().iter().next()),
                Nesting::Owner,
            );
        } else {
            return false;
        }

        self.index_method_reference_for_call(&call_node);
        true
    }

    /// Returns the definition ID of the current nesting (class, module, or singleton class),
    /// but skips methods in the definitions stack.
    fn current_nesting_definition_id(&self) -> Option<DefinitionId> {
        self.nesting_stack.iter().rev().find_map(|nesting| match nesting {
            Nesting::LexicalScope(id) | Nesting::Owner(id) => Some(*id),
            Nesting::Method(_) => None,
        })
    }

    fn current_nesting_is_module(&self) -> bool {
        self.current_nesting_definition_id().is_some_and(|id| {
            self.local_graph
                .definitions()
                .get(&id)
                .is_some_and(|def| matches!(def, Definition::Module(_)))
        })
    }

    /// Indexes the final constant target from a value node, unwrapping chained assignments.
    ///
    /// For `A = B = C`, when processing `A`, the value is `ConstantWriteNode(B)`.
    /// This function recursively unwraps to find the final `ConstantReadNode(C)` and indexes it.
    ///
    /// Returns `Some(NameId)` if the final value is a constant (`ConstantReadNode` or `ConstantPathNode`),
    /// or `None` if the chain ends in a non-constant value.
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

        // Get the location for just the constant name (not including the namespace or value).
        let location = match name_node {
            ruby_prism::Node::ConstantWriteNode { .. } => name_node.as_constant_write_node().unwrap().name_loc(),
            ruby_prism::Node::ConstantOrWriteNode { .. } => name_node.as_constant_or_write_node().unwrap().name_loc(),
            ruby_prism::Node::ConstantPathNode { .. } => name_node.as_constant_path_node().unwrap().name_loc(),
            _ => name_node.location(),
        };

        let offset = Offset::from_prism_location(&location);
        let (comments, flags) = self.find_comments_for(offset.start());
        let lexical_nesting_id = self.parent_lexical_scope_id();

        let alias_constant = ConstantDefinition::new(name_id, self.uri_id, offset, comments, flags, lexical_nesting_id);
        let definition =
            Definition::ConstantAlias(Box::new(ConstantAliasDefinition::new(target_name_id, alias_constant)));
        let definition_id = self.local_graph.add_definition(definition);

        self.add_member_to_current_owner(definition_id);

        Some(definition_id)
    }

    /// Adds a member to the current owner (class, module, or singleton class).
    ///
    /// Iterates through the definitions stack in reverse to find the first class/module/singleton
    /// class, skipping methods, and adds the member to it.
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
            _ => unreachable!("find above only matches anonymous/class/module/singleton"),
        }
    }

    /// Adds a member to the current lexical scope
    ///
    /// Iterates through the definitions stack in reverse to find the first class/module/singleton class, skipping
    /// methods, and adds the member to it. Ignores owner nestings such as Class.new
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
                _ => {}
            }
        }
    }

    /// Indexes a method reference for a call node, creating constant references for the receiver when applicable.
    fn index_method_reference_for_call(&mut self, node: &ruby_prism::CallNode) {
        let method_receiver = self.method_receiver(node.receiver().as_ref(), node.location());

        if method_receiver.is_none()
            && let Some(receiver) = node.receiver()
        {
            self.visit(&receiver);
        }

        let message = String::from_utf8_lossy(node.name().as_slice()).to_string();
        self.index_method_reference(message, &node.message_loc().unwrap(), method_receiver);
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
            Nesting::Owner(_) | Nesting::Method(_) => None,
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
                    Some(Nesting::LexicalScope(id) | Nesting::Owner(id)) => {
                        let definition = self
                            .local_graph
                            .definitions()
                            .get(id)
                            .expect("Nesting definition should exist");

                        match definition {
                            Definition::Class(class_def) => {
                                is_singleton_name = true;
                                Some(*class_def.name_id())
                            }
                            Definition::Module(module_def) => {
                                is_singleton_name = true;
                                Some(*module_def.name_id())
                            }
                            Definition::SingletonClass(singleton_class_def) => {
                                is_singleton_name = true;
                                Some(*singleton_class_def.name_id())
                            }
                            Definition::Method(_) => None,
                            _ => panic!("current nesting is not a class/module/singleton class: {definition:?}"),
                        }
                    }
                    Some(Nesting::Method(id)) => {
                        // If we're inside a method definition, we need to check what its receiver is as that changes the type of `self`
                        let Some(Definition::Method(definition)) = self.local_graph.definitions().get(id) else {
                            unreachable!("method definition for nesting should exist")
                        };

                        if let Some(receiver) = definition.receiver() {
                            is_singleton_name = true;
                            match receiver {
                                Receiver::SelfReceiver(def_id) => self
                                    .local_graph
                                    .definitions()
                                    .get(def_id)
                                    .and_then(Definition::name_id)
                                    .copied(),
                                Receiver::ConstantReceiver(name_id) => Some(*name_id),
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

    fn handle_constant_visibility(&mut self, node: &ruby_prism::CallNode, visibility: Visibility) {
        let receiver = node.receiver();

        let receiver_name_id = match receiver {
            Some(ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. }) => {
                self.index_constant_reference(&receiver.unwrap(), true)
            }
            Some(ruby_prism::Node::SelfNode { .. }) | None => match self.nesting_stack.last() {
                Some(Nesting::Method(_)) => {
                    // Dynamic private constant (called from a method), we ignore it but don't report an error since it's valid Ruby
                    // if being called from a singleton method.

                    return;
                }
                None => {
                    self.local_graph.add_diagnostic(
                        Rule::InvalidPrivateConstant,
                        Offset::from_prism_location(&node.location()),
                        "Private constant called at top level".to_string(),
                    );

                    return;
                }
                _ => None,
            },
            _ => {
                self.local_graph.add_diagnostic(
                    Rule::InvalidPrivateConstant,
                    Offset::from_prism_location(&node.location()),
                    "Dynamic receiver for private constant".to_string(),
                );

                return;
            }
        };

        let Some(arguments) = node.arguments() else {
            return;
        };

        for argument in &arguments.arguments() {
            let (name, location) = match argument {
                ruby_prism::Node::SymbolNode { .. } => {
                    let symbol = argument.as_symbol_node().unwrap();
                    if let Some(value_loc) = symbol.value_loc() {
                        (Self::location_to_string(&value_loc), value_loc)
                    } else {
                        continue;
                    }
                }
                ruby_prism::Node::StringNode { .. } => {
                    let string = argument.as_string_node().unwrap();
                    let name = String::from_utf8_lossy(string.unescaped()).to_string();
                    (name, argument.location())
                }
                _ => {
                    self.local_graph.add_diagnostic(
                        Rule::InvalidPrivateConstant,
                        Offset::from_prism_location(&argument.location()),
                        "Private constant called with non-symbol argument".to_string(),
                    );

                    return;
                }
            };

            let str_id = self.local_graph.intern_string(name);
            let offset = Offset::from_prism_location(&location);
            let definition = Definition::ConstantVisibility(Box::new(ConstantVisibilityDefinition::new(
                self.local_graph.add_name(Name::new(
                    str_id,
                    receiver_name_id.map_or(ParentScope::None, ParentScope::Some),
                    self.current_lexical_scope_name_id(),
                )),
                visibility,
                self.uri_id,
                offset,
                Box::default(),
                DefinitionFlags::empty(),
                self.current_nesting_definition_id(),
            )));

            let definition_id = self.local_graph.add_definition(definition);

            self.add_member_to_current_owner(definition_id);
        }
    }

    fn is_attr_call(arg: &ruby_prism::Node) -> bool {
        arg.as_call_node().is_some_and(|call| {
            let receiver = call.receiver();
            let bare_or_self = receiver.is_none() || receiver.as_ref().is_some_and(|r| r.as_self_node().is_some());
            bare_or_self
                && matches!(
                    call.name().as_slice(),
                    b"attr" | b"attr_reader" | b"attr_writer" | b"attr_accessor"
                )
        })
    }

    /// Classifies each visibility argument and applies visibility left-to-right:
    /// - `DefNode`: inline visibility (always valid)
    /// - Sole attr_* call: inline visibility (multi-arg attr_* is unsupported — returns array)
    /// - `SymbolNode`/`StringNode`: retroactive `MethodVisibilityDefinition`
    /// - Anything else: per-arg diagnostic
    fn handle_visibility_arguments(
        &mut self,
        arguments: &ruby_prism::ArgumentsNode,
        visibility: Visibility,
        call_offset: &Offset,
        call_name: &str,
    ) {
        let args = arguments.arguments();
        let arg_count = args.len();

        for arg in &args {
            if matches!(arg, ruby_prism::Node::DefNode { .. }) || (arg_count == 1 && Self::is_attr_call(&arg)) {
                self.visibility_stack
                    .push(VisibilityModifier::new(visibility, true, call_offset.clone()));
                self.visit(&arg);
                self.visibility_stack.pop();
            } else if matches!(
                arg,
                ruby_prism::Node::SymbolNode { .. } | ruby_prism::Node::StringNode { .. }
            ) {
                self.create_method_visibility_definition(&arg, visibility);
            } else {
                // Unsupported arg — diagnostic + visit for side effects.
                let arg_offset = Offset::from_prism_location(&arg.location());
                let message = if Self::is_attr_call(&arg) {
                    format!("`{call_name}` with `attr_*` is only supported as a single argument")
                } else {
                    format!("`{call_name}` called with a non-literal argument")
                };
                self.local_graph
                    .add_diagnostic(Rule::InvalidMethodVisibility, arg_offset, message);
                self.visit(&arg);
            }
        }
    }

    fn create_method_visibility_definition(&mut self, arg: &ruby_prism::Node, visibility: Visibility) {
        let (name, location) = match arg {
            ruby_prism::Node::SymbolNode { .. } => {
                let symbol = arg.as_symbol_node().unwrap();
                if let Some(value_loc) = symbol.value_loc() {
                    (Self::location_to_string(&value_loc), value_loc)
                } else {
                    return;
                }
            }
            ruby_prism::Node::StringNode { .. } => {
                let string = arg.as_string_node().unwrap();
                let name = String::from_utf8_lossy(string.unescaped()).to_string();
                (name, arg.location())
            }
            _ => return,
        };

        let str_id = self.local_graph.intern_string(format!("{name}()"));
        let arg_offset = Offset::from_prism_location(&location);
        let definition = Definition::MethodVisibility(Box::new(MethodVisibilityDefinition::new(
            str_id,
            visibility,
            self.uri_id,
            arg_offset,
            Box::default(),
            DefinitionFlags::empty(),
            self.current_nesting_definition_id(),
        )));

        let definition_id = self.local_graph.add_definition(definition);
        self.add_member_to_current_owner(definition_id);
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

    fn comments(&self) -> Box<[Comment]> {
        self.comments.clone().into_boxed_slice()
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
            // name_offset points to "self"
            (
                self.current_lexical_scope_name_id(),
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
        let lexical_nesting_id = self.parent_lexical_scope_id();

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
        let nesting = self.current_lexical_scope_name_id();
        let name_id = self
            .local_graph
            .add_name(Name::new(string_id, ParentScope::Attached(attached_target), nesting));

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
        if let Some(target_name_id) = self.index_constant_alias_target(&node.value()) {
            self.add_constant_alias_definition(&node.as_node(), target_name_id, true);
        } else {
            self.add_constant_definition(&node.as_node(), true, Self::is_promotable_value(&node.value()));
            self.visit(&node.value());
        }
    }

    fn visit_constant_write_node(&mut self, node: &ruby_prism::ConstantWriteNode) {
        let value = node.value();
        if self.handle_dynamic_class_or_module(&node.as_node(), &value) {
            return;
        }

        if let Some(target_name_id) = self.index_constant_alias_target(&value) {
            self.add_constant_alias_definition(&node.as_node(), target_name_id, false);
        } else {
            self.add_constant_definition(&node.as_node(), false, Self::is_promotable_value(&value));
            self.visit(&value);
        }
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
        if let Some(target_name_id) = self.index_constant_alias_target(&node.value()) {
            self.add_constant_alias_definition(&node.target().as_node(), target_name_id, true);
        } else {
            self.add_constant_definition(&node.target().as_node(), true, Self::is_promotable_value(&node.value()));
            self.visit(&node.value());
        }
    }

    fn visit_constant_path_write_node(&mut self, node: &ruby_prism::ConstantPathWriteNode) {
        let value = node.value();
        if self.handle_dynamic_class_or_module(&node.as_node(), &value) {
            return;
        }

        if let Some(target_name_id) = self.index_constant_alias_target(&value) {
            self.add_constant_alias_definition(&node.target().as_node(), target_name_id, false);
        } else {
            self.add_constant_definition(&node.target().as_node(), false, Self::is_promotable_value(&value));
            self.visit(&value);
        }
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
                    // Individual values aren't available in multi-write, so we default to
                    // promotable because multi-assignment often comes from meta-programming
                    // (e.g., `A, B = create_classes`).
                    self.add_constant_definition(&left, false, true);
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

        let comment_offset = self
            .take_decorator_offset(offset_for_comments.start())
            .unwrap_or_else(|| offset_for_comments.start());
        let (comments, flags) = self.find_comments_for(comment_offset);

        let receiver = if let Some(recv_node) = node.receiver() {
            match recv_node {
                // def self.foo - receiver is the enclosing definition's DefinitionId
                ruby_prism::Node::SelfNode { .. } => self.current_nesting_definition_id().map(Receiver::SelfReceiver),
                // def Foo.bar or def Foo::Bar.baz - receiver is the constant's NameId
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
            let method = Definition::Method(Box::new(MethodDefinition::new(
                str_id,
                self.uri_id,
                offset.clone(),
                comments.clone(),
                flags.clone(),
                parent_nesting_id,
                Signatures::Simple(parameters.clone().into_boxed_slice()),
                Visibility::Public,
                self.current_nesting_definition_id().map(Receiver::SelfReceiver),
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
                Signatures::Simple(parameters.into_boxed_slice()),
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
                Signatures::Simple(parameters.into_boxed_slice()),
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

        let mut index_attr = |kind: AttrKind, call: &ruby_prism::CallNode| {
            let receiver = call.receiver();
            if receiver.is_some() && receiver.unwrap().as_self_node().is_none() {
                return;
            }

            let call_offset = Offset::from_prism_location(&call.location());

            let current_visibility = self.current_visibility();
            let (visibility, offset_for_comments) = if current_visibility.is_inline() {
                (*current_visibility.visibility(), current_visibility.offset().clone())
            } else {
                (*current_visibility.visibility(), call_offset.clone())
            };

            let comment_offset = self
                .take_decorator_offset(offset_for_comments.start())
                .unwrap_or_else(|| offset_for_comments.start());

            Self::each_string_or_symbol_arg(call, |name, location| {
                let str_id = self.local_graph.intern_string(format!("{name}()"));
                let parent_nesting_id = self.parent_nesting_id();
                let offset = Offset::from_prism_location(&location);

                let (comments, flags) = self.find_comments_for(comment_offset);

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
                let recv_node = node.receiver();
                let recv_ref = recv_node.as_ref();
                if recv_ref.is_some_and(|recv| {
                    !matches!(
                        recv,
                        ruby_prism::Node::SelfNode { .. }
                            | ruby_prism::Node::ConstantReadNode { .. }
                            | ruby_prism::Node::ConstantPathNode { .. }
                    )
                }) {
                    // TODO: Add a diagnostic for dynamic receivers
                    self.visit_call_node_parts(node);
                    return;
                }

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

                let (receiver, method_receiver) = match recv_ref {
                    Some(
                        recv @ (ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. }),
                    ) => {
                        let name_id = self.index_constant_reference(recv, true);
                        (name_id.map(Receiver::ConstantReceiver), name_id)
                    }
                    _ => (None, self.method_receiver(recv_ref, node.location())),
                };
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
                    receiver,
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
                if node.receiver().is_some() {
                    let offset = Offset::from_prism_location(&node.location());
                    self.local_graph.add_diagnostic(
                        Rule::InvalidMethodVisibility,
                        offset,
                        format!("`{message}` cannot be called with an explicit receiver"),
                    );
                    self.visit_call_node_parts(node);
                    return;
                }

                let visibility = Visibility::from_string(message.as_str());
                let offset = Offset::from_prism_location(&node.location());

                if let Some(arguments) = node.arguments() {
                    if visibility == Visibility::ModuleFunction && !self.current_nesting_is_module() {
                        self.local_graph.add_diagnostic(
                            Rule::InvalidMethodVisibility,
                            offset,
                            "`module_function` can only be used in modules".to_string(),
                        );
                        self.visit_arguments_node(&arguments);
                    } else {
                        self.handle_visibility_arguments(&arguments, visibility, &offset, message.as_str());
                    }
                } else {
                    // Flag mode: `private` with no arguments
                    //
                    // Replace the current visibility so it affects all subsequent method definitions.
                    let last_visibility = self.visibility_stack.last_mut().unwrap();
                    *last_visibility = VisibilityModifier::new(visibility, false, offset);
                }
            }
            "new" => {
                let receiver_name = node.receiver().map(|r| r.location().as_slice());

                if matches!(receiver_name, Some(b"Class" | b"::Class")) {
                    self.handle_class_definition(
                        &node.location(),
                        None,
                        node.block(),
                        node.arguments().and_then(|args| args.arguments().iter().next()),
                        Nesting::Owner,
                    );
                } else if matches!(receiver_name, Some(b"Module" | b"::Module")) {
                    self.handle_module_definition(&node.location(), None, node.block(), Nesting::Owner);
                } else {
                    if let Some(arguments) = node.arguments() {
                        self.visit_arguments_node(&arguments);
                    }

                    if let Some(block) = node.block() {
                        self.visit(&block);
                    }
                }

                self.index_method_reference_for_call(node);
            }
            "sig"
                if node.receiver().is_none()
                    || matches!(
                        node.receiver(),
                        Some(ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. })
                    ) =>
            {
                self.pending_decorator_offset = Some(Offset::from_prism_location(&node.location()));

                if let Some(arguments) = node.arguments() {
                    self.visit_arguments_node(&arguments);
                }

                if let Some(block) = node.block() {
                    self.visit(&block);
                }

                self.index_method_reference_for_call(node);
            }
            "private_constant" => {
                self.handle_constant_visibility(node, Visibility::Private);
            }
            "public_constant" => {
                self.handle_constant_visibility(node, Visibility::Public);
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
        let method_receiver = self.method_receiver(node.receiver().as_ref(), node.location());

        if method_receiver.is_none()
            && let Some(receiver) = node.receiver()
        {
            self.visit(&receiver);
        }

        let read_name = String::from_utf8_lossy(node.read_name().as_slice()).to_string();
        self.index_method_reference(read_name, &node.operator_loc(), method_receiver);

        let write_name = String::from_utf8_lossy(node.write_name().as_slice()).to_string();
        self.index_method_reference(write_name, &node.operator_loc(), method_receiver);

        self.visit(&node.value());
    }

    fn visit_call_operator_write_node(&mut self, node: &ruby_prism::CallOperatorWriteNode) {
        let method_receiver = self.method_receiver(node.receiver().as_ref(), node.location());

        if method_receiver.is_none()
            && let Some(receiver) = node.receiver()
        {
            self.visit(&receiver);
        }

        let read_name = String::from_utf8_lossy(node.read_name().as_slice()).to_string();
        self.index_method_reference(read_name, &node.call_operator_loc().unwrap(), method_receiver);

        let write_name = String::from_utf8_lossy(node.write_name().as_slice()).to_string();
        self.index_method_reference(write_name, &node.call_operator_loc().unwrap(), method_receiver);

        self.visit(&node.value());
    }

    fn visit_call_or_write_node(&mut self, node: &ruby_prism::CallOrWriteNode) {
        let method_receiver = self.method_receiver(node.receiver().as_ref(), node.location());

        if method_receiver.is_none()
            && let Some(receiver) = node.receiver()
        {
            self.visit(&receiver);
        }

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
            None,
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
#[path = "ruby_indexer_tests.rs"]
mod tests;
