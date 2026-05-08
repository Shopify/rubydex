//! Visit the Ruby AST and produce an ordered list of operations.
//!
//! Walks the parsed AST and produces `Vec<Operation>` that can later be applied
//! by the applier to create definitions and declarations in a `LocalGraph`.

use std::collections::hash_map::Entry;

use crate::diagnostic::{Diagnostic, Rule};
use crate::model::comment::Comment;
use crate::model::definitions::{DefinitionFlags, Parameter, ParameterStruct, Signatures};
use crate::model::document::Document;
use crate::model::identity_maps::IdentityHashMap;
use crate::model::ids::{NameId, StringId, UriId};
use crate::model::name::{Name, NameRef, ParentScope};
use crate::model::string_ref::StringRef;
use crate::model::visibility::Visibility;
use crate::offset::Offset;
use crate::operation::{self as op, AttrKind, MixinKind, Operation, Target};

use ruby_prism::{ParseResult, Visit};

/// The result of running the operation builder on a Ruby source file.
///
/// Contains the ordered operations and all interning data (strings, names, references)
/// needed to later apply the operations to a graph.
#[derive(Debug)]
pub struct OperationBuilderResult {
    pub uri_id: UriId,
    pub document: Document,
    pub operations: Vec<Operation>,
    pub strings: IdentityHashMap<StringId, StringRef>,
    pub names: IdentityHashMap<NameId, NameRef>,
}

#[derive(Clone, Copy)]
enum Nesting {
    /// A lexical scope (class/module keyword) that produces a new constant resolution scope.
    LexicalScope { name_id: NameId, is_module: bool },
    /// An owner that doesn't produce a lexical scope (Class.new/Module.new).
    Owner { name_id: NameId, is_module: bool },
    /// A method entry, used for instance variable ownership.
    Method { receiver: Option<NestingReceiver> },
}

/// Tracks receiver info for methods on the nesting stack, so `method_receiver` can work
/// without looking up definitions. Distinct from `operation::Target` which represents
/// the source-level receiver without resolved names.
#[derive(Clone, Copy)]
enum NestingReceiver {
    SelfReceiver(NameId),
    ConstantReceiver(NameId),
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
}

/// Visits the Ruby AST and produces an ordered list of operations.
pub struct RubyOperationBuilder<'a> {
    uri_id: UriId,
    source: &'a str,
    // Interning
    strings: IdentityHashMap<StringId, StringRef>,
    names: IdentityHashMap<NameId, NameRef>,
    document: Document,
    // State
    comments: Vec<CommentGroup>,
    nesting_stack: Vec<Nesting>,
    visibility_stack: Vec<VisibilityModifier>,
    pending_decorator_offset: Option<Offset>,
    // Output
    operations: Vec<Operation>,
}

impl<'a> RubyOperationBuilder<'a> {
    #[must_use]
    pub fn new(uri: String, source: &'a str) -> Self {
        let uri_id = UriId::from(&uri);

        Self {
            uri_id,
            source,
            strings: IdentityHashMap::default(),
            names: IdentityHashMap::default(),
            document: Document::new(uri, source),
            comments: Vec::new(),
            nesting_stack: Vec::new(),
            visibility_stack: vec![VisibilityModifier::new(Visibility::Private, false, Offset::new(0, 0))],
            pending_decorator_offset: None,
            operations: Vec::new(),
        }
    }

    #[must_use]
    pub fn build(mut self) -> OperationBuilderResult {
        let result = ruby_prism::parse(self.source.as_bytes());

        for error in result.errors() {
            self.add_diagnostic(
                Rule::ParseError,
                Offset::from_prism_location(&error.location()),
                error.message().to_string(),
            );
        }

        for warning in result.warnings() {
            self.add_diagnostic(
                Rule::ParseWarning,
                Offset::from_prism_location(&warning.location()),
                warning.message().to_string(),
            );
        }

        self.comments = self.parse_comments_into_groups(&result);
        self.visit(&result.node());

        OperationBuilderResult {
            uri_id: self.uri_id,
            document: self.document,
            operations: self.operations,
            strings: self.strings,
            names: self.names,
        }
    }

    // -- Interning --

    fn intern_string(&mut self, string: String) -> StringId {
        let string_id = StringId::from(&string);

        match self.strings.entry(string_id) {
            Entry::Occupied(mut entry) => {
                debug_assert!(string == **entry.get(), "StringId collision");
                entry.get_mut().increment_ref_count(1);
            }
            Entry::Vacant(entry) => {
                entry.insert(StringRef::new(string));
            }
        }

        string_id
    }

    fn add_name(&mut self, name: Name) -> NameId {
        let name_id = name.id();

        match self.names.entry(name_id) {
            Entry::Occupied(mut entry) => {
                debug_assert!(*entry.get() == name, "NameId collision");
                entry.get_mut().increment_ref_count(1);
            }
            Entry::Vacant(entry) => {
                entry.insert(NameRef::Unresolved(Box::new(name)));
            }
        }

        name_id
    }

    fn add_diagnostic(&mut self, rule: Rule, offset: Offset, message: String) {
        let diagnostic = Diagnostic::new(rule, self.uri_id, offset, message);
        self.document.add_diagnostic(diagnostic);
    }

    // -- Nesting helpers --

    fn current_nesting_is_module(&self) -> bool {
        self.nesting_stack
            .iter()
            .rev()
            .find_map(|nesting| match nesting {
                Nesting::LexicalScope { is_module, .. } | Nesting::Owner { is_module, .. } => Some(*is_module),
                Nesting::Method { .. } => None,
            })
            .unwrap_or(false)
    }

    fn current_lexical_scope_name_id(&self) -> Option<NameId> {
        self.nesting_stack.iter().rev().find_map(|nesting| match nesting {
            Nesting::LexicalScope { name_id, .. } => Some(*name_id),
            Nesting::Owner { .. } | Nesting::Method { .. } => None,
        })
    }

    fn current_owner_name_id(&self) -> Option<NameId> {
        self.nesting_stack.iter().rev().find_map(|nesting| match nesting {
            Nesting::LexicalScope { name_id, .. } | Nesting::Owner { name_id, .. } => Some(*name_id),
            Nesting::Method { .. } => None,
        })
    }

    fn current_visibility(&self) -> &VisibilityModifier {
        self.visibility_stack
            .last()
            .expect("visibility stack should not be empty")
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

    fn find_comments_for(&self, offset: u32) -> (Box<[Comment]>, DefinitionFlags) {
        let offset_usize = offset as usize;
        if self.comments.is_empty() {
            return (Box::default(), DefinitionFlags::empty());
        }

        let idx = match self.comments.binary_search_by_key(&offset_usize, |g| g.end_offset) {
            Ok(_) => {
                debug_assert!(false, "Comment ends exactly at definition start");
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

        if bytecount::count(between, b'\n') > 2 {
            return (Box::default(), DefinitionFlags::empty());
        }

        (group.comments(), group.flags())
    }

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

    fn index_constant_reference(&mut self, node: &ruby_prism::Node, push_final_reference: bool) -> Option<NameId> {
        let mut parent_scope_id = ParentScope::None;

        let location = match node {
            ruby_prism::Node::ConstantPathNode { .. } => {
                let constant = node.as_constant_path_node().unwrap();

                if let Some(parent) = constant.parent() {
                    match parent {
                        ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. } => {}
                        _ => {
                            self.add_diagnostic(
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
        let string_id = self.intern_string(name);
        let name_id = self.add_name(Name::new(
            string_id,
            parent_scope_id,
            self.current_lexical_scope_name_id(),
        ));

        if push_final_reference {
            self.operations
                .push(Operation::ReferenceConstant(op::ReferenceConstant {
                    name_id,
                    uri_id: self.uri_id,
                    offset,
                }));
        }

        Some(name_id)
    }

    fn index_method_reference(&mut self, name: String, location: &ruby_prism::Location, receiver: Option<NameId>) {
        let offset = Offset::from_prism_location(location);
        let str_id = self.intern_string(name);
        self.operations.push(Operation::ReferenceMethod(op::ReferenceMethod {
            str_id,
            uri_id: self.uri_id,
            offset,
            receiver: receiver.map(Target::Constant),
        }));
    }

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

    // -- Method receiver resolution --

    fn method_receiver(
        &mut self,
        receiver: Option<&ruby_prism::Node>,
        fallback_location: ruby_prism::Location,
    ) -> Option<NameId> {
        let mut is_singleton_name = false;

        let name_id = match receiver {
            Some(ruby_prism::Node::SelfNode { .. }) | None => match self.nesting_stack.last() {
                Some(Nesting::LexicalScope { name_id, .. } | Nesting::Owner { name_id, .. }) => {
                    is_singleton_name = true;
                    Some(*name_id)
                }
                Some(Nesting::Method { receiver, .. }) => {
                    if let Some(recv) = receiver {
                        is_singleton_name = true;
                        match recv {
                            NestingReceiver::SelfReceiver(name_id) | NestingReceiver::ConstantReceiver(name_id) => {
                                Some(*name_id)
                            }
                        }
                    } else {
                        self.current_owner_name_id()
                    }
                }
                None => {
                    let str_id = self.intern_string("Object".into());
                    Some(self.add_name(Name::new(str_id, ParentScope::None, None)))
                }
            },
            Some(ruby_prism::Node::CallNode { .. }) => {
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
            let name = self.names.get(&name_id).expect("Indexed constant name should exist");

            let target_str = self
                .strings
                .get(name.str())
                .expect("Indexed constant string should exist");

            format!("<{}>", target_str.as_str())
        };

        let string_id = self.intern_string(singleton_class_name);
        let new_name_id = self.add_name(Name::new(string_id, ParentScope::Attached(name_id), None));

        let location = receiver.map_or(fallback_location, ruby_prism::Node::location);
        let offset = Offset::from_prism_location(&location);
        self.operations
            .push(Operation::ReferenceConstant(op::ReferenceConstant {
                name_id: new_name_id,
                uri_id: self.uri_id,
                offset,
            }));
        Some(new_name_id)
    }

    // -- Parameters --

    fn collect_parameters(&mut self, node: &ruby_prism::DefNode) -> Vec<Parameter> {
        let mut parameters: Vec<Parameter> = Vec::new();

        let Some(parameters_list) = node.parameters() else {
            return parameters;
        };

        for parameter in &parameters_list.requireds() {
            let location = parameter.location();
            let str_id = self.intern_string(Self::location_to_string(&location));
            parameters.push(Parameter::RequiredPositional(ParameterStruct::new(
                Offset::from_prism_location(&location),
                str_id,
            )));
        }

        for parameter in &parameters_list.optionals() {
            let opt_param = parameter.as_optional_parameter_node().unwrap();
            let name_loc = opt_param.name_loc();
            let str_id = self.intern_string(Self::location_to_string(&name_loc));
            parameters.push(Parameter::OptionalPositional(ParameterStruct::new(
                Offset::from_prism_location(&name_loc),
                str_id,
            )));
            self.visit(&opt_param.value());
        }

        if let Some(rest) = parameters_list.rest() {
            let rest_param = rest.as_rest_parameter_node().unwrap();
            let location = rest_param.name_loc().unwrap_or_else(|| rest.location());
            let str_id = self.intern_string(Self::location_to_string(&location));
            parameters.push(Parameter::RestPositional(ParameterStruct::new(
                Offset::from_prism_location(&location),
                str_id,
            )));
        }

        for post in &parameters_list.posts() {
            let location = post.location();
            let str_id = self.intern_string(Self::location_to_string(&location));
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
                    let str_id =
                        self.intern_string(Self::location_to_string(&name_loc).trim_end_matches(':').to_string());
                    parameters.push(Parameter::RequiredKeyword(ParameterStruct::new(
                        Offset::from_prism_location(&name_loc),
                        str_id,
                    )));
                }
                ruby_prism::Node::OptionalKeywordParameterNode { .. } => {
                    let optional = keyword.as_optional_keyword_parameter_node().unwrap();
                    let name_loc = optional.name_loc();
                    let str_id =
                        self.intern_string(Self::location_to_string(&name_loc).trim_end_matches(':').to_string());
                    parameters.push(Parameter::OptionalKeyword(ParameterStruct::new(
                        Offset::from_prism_location(&name_loc),
                        str_id,
                    )));
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
                    let str_id = self.intern_string(Self::location_to_string(&location));
                    parameters.push(Parameter::RestKeyword(ParameterStruct::new(
                        Offset::from_prism_location(&location),
                        str_id,
                    )));
                }
                ruby_prism::Node::ForwardingParameterNode { .. } => {
                    let location = rest.location();
                    let str_id = self.intern_string(Self::location_to_string(&location));
                    parameters.push(Parameter::Forward(ParameterStruct::new(
                        Offset::from_prism_location(&location),
                        str_id,
                    )));
                }
                _ => {}
            }
        }

        if let Some(block) = parameters_list.block() {
            let location = block.name_loc().unwrap_or_else(|| block.location());
            let str_id = self.intern_string(Self::location_to_string(&location));
            parameters.push(Parameter::Block(ParameterStruct::new(
                Offset::from_prism_location(&location),
                str_id,
            )));
        }

        parameters
    }

    // -- Helpers --

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

    fn is_promotable_value(value: &ruby_prism::Node) -> bool {
        value
            .as_call_node()
            .is_some_and(|call| call.receiver().is_none() || call.call_operator_loc().is_some())
    }

    // -- Definition handlers --

    fn handle_class_definition(
        &mut self,
        location: &ruby_prism::Location,
        name_node: Option<&ruby_prism::Node>,
        body_node: Option<ruby_prism::Node>,
        superclass_node: Option<ruby_prism::Node>,
        is_lexical_scope: bool,
    ) {
        let offset = Offset::from_prism_location(location);
        let (comments, flags) = self.find_comments_for(offset.start());
        let superclass_name = superclass_node.as_ref().and_then(|n| {
            if let Some(id) = self.index_constant_reference(n, false) {
                self.operations
                    .push(Operation::ReferenceConstant(op::ReferenceConstant {
                        name_id: id,
                        uri_id: self.uri_id,
                        offset: Offset::from_prism_location(&n.location()),
                    }));
                return Some(id);
            }

            if let ruby_prism::Node::CallNode { .. } = n {
                let call = n.as_call_node().unwrap();
                if let Some(receiver) = call.receiver()
                    && let Some(id) = self.index_constant_reference(&receiver, false)
                {
                    self.operations
                        .push(Operation::ReferenceConstant(op::ReferenceConstant {
                            name_id: id,
                            uri_id: self.uri_id,
                            offset: Offset::from_prism_location(&receiver.location()),
                        }));
                    return Some(id);
                }
            }

            None
        });

        if let Some(superclass_node) = superclass_node
            && superclass_name.is_none()
        {
            self.add_diagnostic(
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
            let string_id = self.intern_string(format!("{}:{}<anonymous>", self.uri_id, offset.start()));
            (
                Some(self.add_name(Name::new(string_id, ParentScope::None, None))),
                offset.clone(),
            )
        };

        if let Some(name_id) = name_id {
            self.operations.push(Operation::EnterClass(op::EnterClass {
                name_id,
                uri_id: self.uri_id,
                offset: offset.clone(),
                name_offset,
                comments,
                flags,
                superclass_name,
                is_lexical_scope,
            }));

            let nesting = if is_lexical_scope {
                Nesting::LexicalScope {
                    name_id,
                    is_module: false,
                }
            } else {
                Nesting::Owner {
                    name_id,
                    is_module: false,
                }
            };
            self.nesting_stack.push(nesting);
            self.visibility_stack
                .push(VisibilityModifier::new(Visibility::Public, false, offset));
            if let Some(body) = body_node {
                self.visit(&body);
            }
            self.visibility_stack.pop();
            self.nesting_stack.pop();
            self.operations.push(Operation::ExitScope);
        }
    }

    fn handle_module_definition(
        &mut self,
        location: &ruby_prism::Location,
        name_node: Option<&ruby_prism::Node>,
        body_node: Option<ruby_prism::Node>,
        is_lexical_scope: bool,
    ) {
        let offset = Offset::from_prism_location(location);
        let (comments, flags) = self.find_comments_for(offset.start());

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
            let string_id = self.intern_string(format!("{}:{}<anonymous>", self.uri_id, offset.start()));
            (
                Some(self.add_name(Name::new(string_id, ParentScope::None, None))),
                offset.clone(),
            )
        };

        if let Some(name_id) = name_id {
            self.operations.push(Operation::EnterModule(op::EnterModule {
                name_id,
                uri_id: self.uri_id,
                offset: offset.clone(),
                name_offset,
                comments,
                flags,
                is_lexical_scope,
            }));

            let nesting = if is_lexical_scope {
                Nesting::LexicalScope {
                    name_id,
                    is_module: true,
                }
            } else {
                Nesting::Owner {
                    name_id,
                    is_module: true,
                }
            };
            self.nesting_stack.push(nesting);
            self.visibility_stack
                .push(VisibilityModifier::new(Visibility::Public, false, offset));
            if let Some(body) = body_node {
                self.visit(&body);
            }
            self.visibility_stack.pop();
            self.nesting_stack.pop();
            self.operations.push(Operation::ExitScope);
        }
    }

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
            self.handle_module_definition(&node.location(), Some(node), call_node.block(), false);
        } else if matches!(receiver_name, b"Class" | b"::Class") {
            self.handle_class_definition(
                &node.location(),
                Some(node),
                call_node.block(),
                call_node.arguments().and_then(|args| args.arguments().iter().next()),
                false,
            );
        } else {
            return false;
        }

        self.index_method_reference_for_call(&call_node);
        true
    }

    fn handle_mixin(&mut self, node: &ruby_prism::CallNode, kind: MixinKind) {
        let Some(arguments) = node.arguments() else {
            return;
        };

        let has_owner = self.current_owner_name_id().is_some();

        let mixin_arguments = arguments
            .arguments()
            .iter()
            .filter_map(|arg| {
                if arg.as_self_node().is_some() {
                    if !has_owner {
                        self.add_diagnostic(
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
                    self.add_diagnostic(
                        Rule::DynamicAncestor,
                        Offset::from_prism_location(&arg.location()),
                        "Dynamic mixin argument".to_string(),
                    );
                    None
                }
            })
            .collect::<Vec<(NameId, Offset)>>();

        if mixin_arguments.is_empty() || !has_owner {
            return;
        }

        // Mixin operations with multiple arguments are inserted in reverse
        for (name_id, offset) in mixin_arguments.into_iter().rev() {
            self.operations
                .push(Operation::ReferenceConstant(op::ReferenceConstant {
                    name_id,
                    uri_id: self.uri_id,
                    offset,
                }));

            self.operations.push(Operation::Mixin(op::Mixin {
                kind,
                target: Target::Constant(name_id),
            }));
        }
    }

    fn handle_constant_visibility(&mut self, node: &ruby_prism::CallNode, visibility: Visibility) {
        let receiver = node.receiver();

        let receiver_name_id = match receiver {
            Some(ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. }) => {
                self.index_constant_reference(&receiver.unwrap(), true)
            }
            Some(ruby_prism::Node::SelfNode { .. }) | None => match self.nesting_stack.last() {
                Some(Nesting::Method { .. }) => return,
                None => {
                    self.add_diagnostic(
                        Rule::InvalidPrivateConstant,
                        Offset::from_prism_location(&node.location()),
                        "Private constant called at top level".to_string(),
                    );
                    return;
                }
                _ => None,
            },
            _ => {
                self.add_diagnostic(
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
                    self.add_diagnostic(
                        Rule::InvalidPrivateConstant,
                        Offset::from_prism_location(&argument.location()),
                        "Private constant called with non-symbol argument".to_string(),
                    );
                    continue;
                }
            };

            let str_id = self.intern_string(name);
            let offset = Offset::from_prism_location(&location);

            self.operations
                .push(Operation::SetConstantVisibility(op::SetConstantVisibility {
                    receiver: receiver_name_id.map(Target::Constant),
                    target: str_id,
                    visibility,
                    uri_id: self.uri_id,
                    offset,
                    comments: Box::default(),
                    flags: DefinitionFlags::empty(),
                }));
        }
    }

    // -- Constant definition helpers --

    fn add_constant_definition(
        &mut self,
        node: &ruby_prism::Node,
        also_add_reference: bool,
        promotable: bool,
    ) -> Option<()> {
        let name_id = self.index_constant_reference(node, also_add_reference)?;

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
        self.operations.push(Operation::DefineConstant(op::DefineConstant {
            name_id,
            uri_id: self.uri_id,
            offset,
            comments,
            flags,
        }));

        Some(())
    }

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
    ) -> Option<()> {
        let name_id = self.index_constant_reference(name_node, also_add_reference)?;

        let location = match name_node {
            ruby_prism::Node::ConstantWriteNode { .. } => name_node.as_constant_write_node().unwrap().name_loc(),
            ruby_prism::Node::ConstantOrWriteNode { .. } => name_node.as_constant_or_write_node().unwrap().name_loc(),
            ruby_prism::Node::ConstantPathNode { .. } => name_node.as_constant_path_node().unwrap().name_loc(),
            _ => name_node.location(),
        };

        let offset = Offset::from_prism_location(&location);
        let (comments, flags) = self.find_comments_for(offset.start());

        self.operations.push(Operation::AliasConstant(op::AliasConstant {
            name_id,
            target_name_id,
            uri_id: self.uri_id,
            offset,
            comments,
            flags,
        }));

        Some(())
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
                let previous_visibility = self.current_visibility().visibility;

                self.operations
                    .push(Operation::SetDefaultVisibility(op::SetDefaultVisibility {
                        visibility,
                        uri_id: self.uri_id,
                        offset: call_offset.clone(),
                    }));

                self.visibility_stack
                    .push(VisibilityModifier::new(visibility, true, call_offset.clone()));
                self.visit(&arg);
                self.visibility_stack.pop();

                self.operations
                    .push(Operation::SetDefaultVisibility(op::SetDefaultVisibility {
                        visibility: previous_visibility,
                        uri_id: self.uri_id,
                        offset: call_offset.clone(),
                    }));
            } else if matches!(
                arg,
                ruby_prism::Node::SymbolNode { .. } | ruby_prism::Node::StringNode { .. }
            ) {
                self.create_method_visibility_operation(&arg, visibility, DefinitionFlags::empty());
            } else {
                let arg_offset = Offset::from_prism_location(&arg.location());
                let message = if Self::is_attr_call(&arg) {
                    format!("`{call_name}` with `attr_*` is only supported as a single argument")
                } else {
                    format!("`{call_name}` called with a non-literal argument")
                };
                self.add_diagnostic(Rule::InvalidMethodVisibility, arg_offset, message);
                self.visit(&arg);
            }
        }
    }

    fn create_method_visibility_operation(
        &mut self,
        arg: &ruby_prism::Node,
        visibility: Visibility,
        flags: DefinitionFlags,
    ) {
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

        let str_id = self.intern_string(format!("{name}()"));
        let offset = Offset::from_prism_location(&location);

        self.operations
            .push(Operation::SetMethodVisibility(op::SetMethodVisibility {
                str_id,
                visibility,
                uri_id: self.uri_id,
                offset,
                flags,
            }));
    }

    fn create_method_visibility_operation_from_name(
        &mut self,
        name: &str,
        location: &ruby_prism::Location,
        visibility: Visibility,
        flags: DefinitionFlags,
    ) {
        let str_id = self.intern_string(format!("{name}()"));
        let offset = Offset::from_prism_location(location);

        self.operations
            .push(Operation::SetMethodVisibility(op::SetMethodVisibility {
                str_id,
                visibility,
                uri_id: self.uri_id,
                offset,
                flags,
            }));
    }

    #[allow(clippy::too_many_lines)]
    fn handle_singleton_method_visibility(
        &mut self,
        node: &ruby_prism::CallNode,
        visibility: Visibility,
        call_name: &str,
    ) {
        match node.receiver() {
            Some(ruby_prism::Node::SelfNode { .. }) | None => match self.nesting_stack.last() {
                Some(Nesting::Method { .. }) => {
                    self.visit_call_node_parts(node);
                    return;
                }
                None => {
                    self.add_diagnostic(
                        Rule::InvalidMethodVisibility,
                        Offset::from_prism_location(&node.location()),
                        format!("`{call_name}` called at top level"),
                    );
                    self.visit_call_node_parts(node);
                    return;
                }
                _ => {}
            },
            _ => {
                self.visit_call_node_parts(node);
                return;
            }
        }

        let Some(arguments) = node.arguments() else {
            return;
        };

        let args = arguments.arguments();
        let arg_count = args.len();

        for argument in &args {
            match argument {
                ruby_prism::Node::SymbolNode { .. } | ruby_prism::Node::StringNode { .. } => {
                    self.create_method_visibility_operation(
                        &argument,
                        visibility,
                        DefinitionFlags::SINGLETON_METHOD_VISIBILITY,
                    );
                }
                ruby_prism::Node::ArrayNode { .. } if arg_count == 1 => {
                    let array = argument.as_array_node().unwrap();
                    for element in &array.elements() {
                        match element {
                            ruby_prism::Node::SymbolNode { .. } | ruby_prism::Node::StringNode { .. } => {
                                self.create_method_visibility_operation(
                                    &element,
                                    visibility,
                                    DefinitionFlags::SINGLETON_METHOD_VISIBILITY,
                                );
                            }
                            ruby_prism::Node::DefNode { .. } => {
                                let def_node = element.as_def_node().unwrap();
                                if def_node.receiver().is_none() {
                                    self.add_diagnostic(
                                        Rule::InvalidMethodVisibility,
                                        Offset::from_prism_location(&element.location()),
                                        format!("`{call_name}` requires a singleton method definition"),
                                    );
                                    self.visit(&element);
                                    continue;
                                }
                                let name_loc = def_node.name_loc();
                                let name = Self::location_to_string(&name_loc);
                                self.create_method_visibility_operation_from_name(
                                    &name,
                                    &name_loc,
                                    visibility,
                                    DefinitionFlags::SINGLETON_METHOD_VISIBILITY,
                                );
                                self.visit(&element);
                            }
                            _ => {
                                self.add_diagnostic(
                                    Rule::InvalidMethodVisibility,
                                    Offset::from_prism_location(&element.location()),
                                    format!(
                                        "`{call_name}` array element must be a Symbol, String, or method definition"
                                    ),
                                );
                                self.visit(&element);
                            }
                        }
                    }
                }
                ruby_prism::Node::DefNode { .. } => {
                    let def_node = argument.as_def_node().unwrap();
                    if def_node.receiver().is_none() {
                        self.add_diagnostic(
                            Rule::InvalidMethodVisibility,
                            Offset::from_prism_location(&argument.location()),
                            format!("`{call_name}` requires a singleton method definition"),
                        );
                        self.visit(&argument);
                        continue;
                    }
                    let name_loc = def_node.name_loc();
                    let name = Self::location_to_string(&name_loc);
                    self.create_method_visibility_operation_from_name(
                        &name,
                        &name_loc,
                        visibility,
                        DefinitionFlags::SINGLETON_METHOD_VISIBILITY,
                    );
                    self.visit(&argument);
                }
                arg if Self::is_attr_call(&arg) => {
                    self.add_diagnostic(
                        Rule::InvalidMethodVisibility,
                        Offset::from_prism_location(&arg.location()),
                        format!("`{call_name}` does not accept `attr_*` arguments"),
                    );
                    self.visit(&arg);
                }
                ruby_prism::Node::ArrayNode { .. } => {
                    self.add_diagnostic(
                        Rule::InvalidMethodVisibility,
                        Offset::from_prism_location(&argument.location()),
                        format!("`{call_name}` array argument must be the only argument"),
                    );
                    self.visit(&argument);
                }
                _ => {
                    self.add_diagnostic(
                        Rule::InvalidMethodVisibility,
                        Offset::from_prism_location(&argument.location()),
                        format!("`{call_name}` called with a non-literal argument"),
                    );
                    self.visit(&argument);
                }
            }
        }
    }

    fn add_global_variable_definition(&mut self, location: &ruby_prism::Location) {
        let name = Self::location_to_string(location);
        let str_id = self.intern_string(name);
        let offset = Offset::from_prism_location(location);
        let (comments, flags) = self.find_comments_for(offset.start());

        self.operations
            .push(Operation::DefineGlobalVariable(op::DefineGlobalVariable {
                str_id,
                uri_id: self.uri_id,
                offset,
                comments,
                flags,
            }));
    }

    fn add_instance_variable_definition(&mut self, location: &ruby_prism::Location) {
        let name = Self::location_to_string(location);
        let str_id = self.intern_string(name);
        let offset = Offset::from_prism_location(location);
        let (comments, flags) = self.find_comments_for(offset.start());

        self.operations
            .push(Operation::DefineInstanceVariable(op::DefineInstanceVariable {
                str_id,
                uri_id: self.uri_id,
                offset,
                comments,
                flags,
            }));
    }

    fn add_class_variable_definition(&mut self, location: &ruby_prism::Location) {
        let name = Self::location_to_string(location);
        let str_id = self.intern_string(name);
        let offset = Offset::from_prism_location(location);
        let (comments, flags) = self.find_comments_for(offset.start());

        self.operations
            .push(Operation::DefineClassVariable(op::DefineClassVariable {
                str_id,
                uri_id: self.uri_id,
                offset,
                comments,
                flags,
            }));
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

    fn accepts(&self, next: &ruby_prism::Comment, source: &str) -> bool {
        let current_end_offset = self.end_offset;
        let next_line_start_offset = next.location().start_offset();
        let between = &source.as_bytes()[current_end_offset..next_line_start_offset];
        if !between.iter().all(|&b| b.is_ascii_whitespace()) {
            return false;
        }
        bytecount::count(between, b'\n') <= 1
    }

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

// -- Visit implementation --

impl Visit<'_> for RubyOperationBuilder<'_> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'_>) {
        self.handle_class_definition(
            &node.location(),
            Some(&node.constant_path()),
            node.body(),
            node.superclass(),
            true,
        );
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode) {
        self.handle_module_definition(&node.location(), Some(&node.constant_path()), node.body(), true);
    }

    fn visit_singleton_class_node(&mut self, node: &ruby_prism::SingletonClassNode) {
        let expression = node.expression();

        let (attached_target, name_offset) = if expression.as_self_node().is_some() {
            (
                self.current_lexical_scope_name_id(),
                Offset::from_prism_location(&expression.location()),
            )
        } else if matches!(
            expression,
            ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. }
        ) {
            (
                self.index_constant_reference(&expression, true),
                Offset::from_prism_location(&expression.location()),
            )
        } else {
            self.visit(&expression);
            self.add_diagnostic(
                Rule::DynamicSingletonDefinition,
                Offset::from_prism_location(&node.location()),
                "Dynamic singleton class definition".to_string(),
            );
            return;
        };

        let Some(attached_target) = attached_target else {
            self.add_diagnostic(
                Rule::DynamicSingletonDefinition,
                Offset::from_prism_location(&node.location()),
                "Dynamic singleton class definition".to_string(),
            );
            return;
        };

        let offset = Offset::from_prism_location(&node.location());
        let (comments, flags) = self.find_comments_for(offset.start());

        let singleton_class_name = {
            let name = self
                .names
                .get(&attached_target)
                .expect("Attached target name should exist");
            let target_str = self
                .strings
                .get(name.str())
                .expect("Attached target string should exist");
            format!("<{}>", target_str.as_str())
        };

        let string_id = self.intern_string(singleton_class_name);
        let nesting = self.current_lexical_scope_name_id();
        let name_id = self.add_name(Name::new(string_id, ParentScope::Attached(attached_target), nesting));
        self.operations
            .push(Operation::EnterSingletonClass(op::EnterSingletonClass {
                name_id,
                uri_id: self.uri_id,
                offset: offset.clone(),
                name_offset,
                comments,
                flags,
            }));

        self.nesting_stack.push(Nesting::LexicalScope {
            name_id,
            is_module: false,
        });
        self.visibility_stack
            .push(VisibilityModifier::new(Visibility::Public, false, offset));
        if let Some(body) = node.body() {
            self.visit(&body);
        }
        self.visibility_stack.pop();
        self.nesting_stack.pop();
        self.operations.push(Operation::ExitScope);
    }

    #[allow(clippy::too_many_lines)]
    fn visit_def_node(&mut self, node: &ruby_prism::DefNode) {
        let name = Self::location_to_string(&node.name_loc());
        let str_id = self.intern_string(format!("{name}()"));
        let offset = Offset::from_prism_location(&node.location());
        let parameters = self.collect_parameters(node);
        let is_singleton = node.receiver().is_some();

        let current_visibility = self.current_visibility();
        let visibility = if is_singleton {
            Visibility::Public
        } else {
            current_visibility.visibility
        };
        let offset_for_comments = if is_singleton {
            offset.clone()
        } else if current_visibility.is_inline {
            current_visibility.offset.clone()
        } else {
            offset.clone()
        };

        let comment_offset = self
            .take_decorator_offset(offset_for_comments.start())
            .unwrap_or_else(|| offset_for_comments.start());
        let (comments, flags) = self.find_comments_for(comment_offset);

        let (receiver, method_nesting_receiver) = if let Some(recv_node) = node.receiver() {
            match recv_node {
                ruby_prism::Node::SelfNode { .. } => {
                    let nesting_name = self.current_owner_name_id();
                    (
                        Some(Target::ExplicitSelf),
                        nesting_name.map(NestingReceiver::SelfReceiver),
                    )
                }
                ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. } => {
                    let name_id = self.index_constant_reference(&recv_node, true);
                    (
                        name_id.map(Target::Constant),
                        name_id.map(NestingReceiver::ConstantReceiver),
                    )
                }
                _ => {
                    self.add_diagnostic(
                        Rule::DynamicSingletonDefinition,
                        Offset::from_prism_location(&node.location()),
                        "Dynamic receiver for singleton method definition".to_string(),
                    );
                    self.visit(&recv_node);
                    return;
                }
            }
        } else {
            (None, None)
        };

        if receiver.is_none() && visibility == Visibility::ModuleFunction {
            // module_function: emit two EnterMethod/ExitScope pairs (singleton + instance),
            // each visiting the body so ivars are associated with both methods.
            let singleton_receiver = Some(Target::ExplicitSelf);
            let body = node.body();

            self.operations.push(Operation::EnterMethod(op::EnterMethod {
                str_id,
                uri_id: self.uri_id,
                offset: offset.clone(),
                comments: comments.clone(),
                flags: flags.clone(),
                signatures: Signatures::Simple(parameters.clone().into_boxed_slice()),
                receiver: singleton_receiver,
            }));
            self.nesting_stack.push(Nesting::Method {
                receiver: method_nesting_receiver,
            });
            if let Some(ref body) = body {
                self.visit(body);
            }
            self.nesting_stack.pop();
            self.operations.push(Operation::ExitScope);

            self.operations.push(Operation::EnterMethod(op::EnterMethod {
                str_id,
                uri_id: self.uri_id,
                offset: offset.clone(),
                comments,
                flags,
                signatures: Signatures::Simple(parameters.into_boxed_slice()),
                receiver,
            }));
            self.nesting_stack.push(Nesting::Method {
                receiver: method_nesting_receiver,
            });
            if let Some(ref body) = body {
                self.visit(body);
            }
            self.nesting_stack.pop();
            self.operations.push(Operation::ExitScope);
        } else {
            // Singleton methods at top level have receiver=None (no class to point self to).
            // Bracket with SetDefaultVisibility(Public) so the applier assigns the correct visibility.
            let needs_singleton_visibility_bracket = is_singleton && receiver.is_none();
            let previous_visibility = if needs_singleton_visibility_bracket {
                let prev = self.current_visibility().visibility;
                self.operations
                    .push(Operation::SetDefaultVisibility(op::SetDefaultVisibility {
                        visibility: Visibility::Public,
                        uri_id: self.uri_id,
                        offset: offset.clone(),
                    }));
                Some(prev)
            } else {
                None
            };

            self.operations.push(Operation::EnterMethod(op::EnterMethod {
                str_id,
                uri_id: self.uri_id,
                offset: offset.clone(),
                comments,
                flags,
                signatures: Signatures::Simple(parameters.into_boxed_slice()),
                receiver,
            }));
            self.nesting_stack.push(Nesting::Method {
                receiver: method_nesting_receiver,
            });
            if let Some(body) = node.body() {
                self.visit(&body);
            }
            self.nesting_stack.pop();
            self.operations.push(Operation::ExitScope);

            if let Some(prev) = previous_visibility {
                self.operations
                    .push(Operation::SetDefaultVisibility(op::SetDefaultVisibility {
                        visibility: prev,
                        uri_id: self.uri_id,
                        offset: offset.clone(),
                    }));
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
                    self.add_constant_definition(&left, false, true);
                }
                ruby_prism::Node::GlobalVariableTargetNode { .. } => {
                    self.add_global_variable_definition(&left.location());
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

    #[allow(clippy::too_many_lines)]
    fn visit_call_node(&mut self, node: &ruby_prism::CallNode) {
        let index_attr = |kind: AttrKind, call: &ruby_prism::CallNode, builder: &mut Self| {
            let receiver = call.receiver();
            if receiver.is_some() && receiver.unwrap().as_self_node().is_none() {
                return;
            }

            let call_offset = Offset::from_prism_location(&call.location());

            let current_visibility = builder.current_visibility();
            let offset_for_comments = if current_visibility.is_inline {
                current_visibility.offset.clone()
            } else {
                call_offset
            };

            let comment_offset = builder
                .take_decorator_offset(offset_for_comments.start())
                .unwrap_or_else(|| offset_for_comments.start());

            Self::each_string_or_symbol_arg(call, |name, location| {
                let str_id = builder.intern_string(format!("{name}()"));
                let offset = Offset::from_prism_location(&location);
                let (comments, flags) = builder.find_comments_for(comment_offset);

                builder.operations.push(Operation::DefineAttribute(op::DefineAttribute {
                    kind,
                    str_id,
                    uri_id: builder.uri_id,
                    offset,
                    comments,
                    flags,
                }));
            });
        };

        let message_loc = node.message_loc();
        if message_loc.is_none() {
            return;
        }

        let message = String::from_utf8_lossy(node.name().as_slice()).to_string();

        match message.as_str() {
            "attr_accessor" => {
                index_attr(AttrKind::Accessor, node, self);
            }
            "attr_reader" => {
                index_attr(AttrKind::Reader, node, self);
            }
            "attr_writer" => {
                index_attr(AttrKind::Writer, node, self);
            }
            "attr" => {
                let create_writer = if let Some(arguments) = node.arguments() {
                    let args_vec: Vec<_> = arguments.arguments().iter().collect();
                    matches!(args_vec.as_slice(), [_, ruby_prism::Node::TrueNode { .. }])
                } else {
                    false
                };

                if create_writer {
                    index_attr(AttrKind::Accessor, node, self);
                } else {
                    index_attr(AttrKind::Reader, node, self);
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
                    self.visit_call_node_parts(node);
                    return;
                }

                let mut names: Vec<(String, Offset)> = Vec::new();
                Self::each_string_or_symbol_arg(node, |name, location| {
                    names.push((name, Offset::from_prism_location(&location)));
                });

                if names.len() != 2 {
                    return;
                }

                let (new_name, _new_offset) = &names[0];
                let (old_name, old_offset) = &names[1];

                let new_name_str_id = self.intern_string(format!("{new_name}()"));
                let old_name_str_id = self.intern_string(format!("{old_name}()"));

                let (receiver, method_receiver) = match recv_ref {
                    Some(
                        recv @ (ruby_prism::Node::ConstantPathNode { .. } | ruby_prism::Node::ConstantReadNode { .. }),
                    ) => {
                        let name_id = self.index_constant_reference(recv, true);
                        (name_id.map(Target::Constant), name_id)
                    }
                    _ => (None, self.method_receiver(recv_ref, node.location())),
                };

                let ref_str_id = self.intern_string(format!("{old_name}()"));
                self.operations.push(Operation::ReferenceMethod(op::ReferenceMethod {
                    str_id: ref_str_id,
                    uri_id: self.uri_id,
                    offset: old_offset.clone(),
                    receiver: method_receiver.map(Target::Constant),
                }));

                let offset = Offset::from_prism_location(&node.location());
                let (comments, flags) = self.find_comments_for(offset.start());

                self.operations.push(Operation::AliasMethod(op::AliasMethod {
                    new_name_str_id,
                    old_name_str_id,
                    uri_id: self.uri_id,
                    offset,
                    comments,
                    flags,
                    receiver,
                }));
            }
            "include" => {
                let receiver = node.receiver();
                if receiver.is_none() || receiver.as_ref().is_some_and(|r| r.as_self_node().is_some()) {
                    self.handle_mixin(node, MixinKind::Include);
                } else {
                    self.visit_call_node_parts(node);
                }
            }
            "prepend" => {
                let receiver = node.receiver();
                if receiver.is_none() || receiver.as_ref().is_some_and(|r| r.as_self_node().is_some()) {
                    self.handle_mixin(node, MixinKind::Prepend);
                } else {
                    self.visit_call_node_parts(node);
                }
            }
            "extend" => {
                let receiver = node.receiver();
                if receiver.is_none() || receiver.as_ref().is_some_and(|r| r.as_self_node().is_some()) {
                    self.handle_mixin(node, MixinKind::Extend);
                } else {
                    self.visit_call_node_parts(node);
                }
            }
            "private" | "protected" | "public" | "module_function" => {
                if node.receiver().is_some() {
                    let offset = Offset::from_prism_location(&node.location());
                    self.add_diagnostic(
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
                        self.add_diagnostic(
                            Rule::InvalidMethodVisibility,
                            offset,
                            "`module_function` can only be used in modules".to_string(),
                        );
                        self.visit_arguments_node(&arguments);
                    } else {
                        self.handle_visibility_arguments(&arguments, visibility, &offset, &message);
                    }
                } else {
                    let last_visibility = self.visibility_stack.last_mut().unwrap();
                    *last_visibility = VisibilityModifier::new(visibility, false, offset);
                    self.operations
                        .push(Operation::SetDefaultVisibility(op::SetDefaultVisibility {
                            visibility,
                            uri_id: self.uri_id,
                            offset: Offset::from_prism_location(&node.location()),
                        }));
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
                        false,
                    );
                } else if matches!(receiver_name, Some(b"Module" | b"::Module")) {
                    self.handle_module_definition(&node.location(), None, node.block(), false);
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
            "private_class_method" => {
                self.handle_singleton_method_visibility(node, Visibility::Private, "private_class_method");
            }
            "public_class_method" => {
                self.handle_singleton_method_visibility(node, Visibility::Public, "public_class_method");
            }
            _ => {
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
        self.add_global_variable_definition(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_global_variable_and_write_node(&mut self, node: &ruby_prism::GlobalVariableAndWriteNode<'_>) {
        self.add_global_variable_definition(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_global_variable_or_write_node(&mut self, node: &ruby_prism::GlobalVariableOrWriteNode<'_>) {
        self.add_global_variable_definition(&node.name_loc());
        self.visit(&node.value());
    }

    fn visit_global_variable_operator_write_node(&mut self, node: &ruby_prism::GlobalVariableOperatorWriteNode<'_>) {
        self.add_global_variable_definition(&node.name_loc());
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
        let new_name_str_id = self.intern_string(new_name);
        let old_name_str_id = self.intern_string(old_name.clone());

        self.operations.push(Operation::AliasMethod(op::AliasMethod {
            new_name_str_id,
            old_name_str_id,
            uri_id: self.uri_id,
            offset,
            comments,
            flags,
            receiver: None,
        }));

        self.index_method_reference(old_name, &node.old_name().location(), None);
    }

    fn visit_alias_global_variable_node(&mut self, node: &ruby_prism::AliasGlobalVariableNode<'_>) {
        let new_name = Self::location_to_string(&node.new_name().location());
        let old_name = Self::location_to_string(&node.old_name().location());
        let new_name_str_id = self.intern_string(new_name);
        let old_name_str_id = self.intern_string(old_name);
        let offset = Offset::from_prism_location(&node.location());
        let (comments, flags) = self.find_comments_for(offset.start());

        self.operations
            .push(Operation::AliasGlobalVariable(op::AliasGlobalVariable {
                new_name_str_id,
                old_name_str_id,
                uri_id: self.uri_id,
                offset,
                comments,
                flags,
            }));
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
    use super::*;
    use crate::operation::printer;

    fn build_operations(source: &str) -> OperationBuilderResult {
        let source = crate::test_utils::normalize_indentation(source);
        let builder = RubyOperationBuilder::new("file:///test.rb".to_string(), &source);
        builder.build()
    }

    fn normalize_expected(expected: &str) -> String {
        crate::test_utils::normalize_indentation(expected).trim().to_string()
    }

    fn assert_operations(source: &str, expected: &str) {
        let result = build_operations(source);
        let actual = printer::print_operations(&result.operations, &result.strings, &result.names, false);
        let expected = normalize_expected(expected);
        assert_eq!(actual, expected, "\n\nActual:\n{actual}\n\nExpected:\n{expected}\n");
    }

    fn assert_operations_with_references(source: &str, expected: &str) {
        let result = build_operations(source);
        let actual = printer::print_operations(&result.operations, &result.strings, &result.names, true);
        let expected = normalize_expected(expected);
        assert_eq!(actual, expected, "\n\nActual:\n{actual}\n\nExpected:\n{expected}\n");
    }

    // -- Namespace tests --

    #[test]
    fn build_class_node() {
        assert_operations(
            "
            class Foo
              class Bar; end
            end
            ",
            "
            EnterClass(Foo)
              EnterClass(Bar)
              ExitScope
            ExitScope
            ",
        );
    }

    #[test]
    fn build_class_with_qualified_name() {
        assert_operations(
            "
            class Foo::Bar; end
            ",
            "
            EnterClass(Foo::Bar)
            ExitScope
            ",
        );
    }

    #[test]
    fn build_class_with_superclass() {
        assert_operations(
            "
            class Foo < Bar; end
            ",
            "
            EnterClass(Foo, superclass: Bar)
            ExitScope
            ",
        );
    }

    #[test]
    fn build_module_node() {
        assert_operations(
            "
            module Foo
              module Bar; end
            end
            ",
            "
            EnterModule(Foo)
              EnterModule(Bar)
              ExitScope
            ExitScope
            ",
        );
    }

    #[test]
    fn build_singleton_class() {
        assert_operations(
            "
            class Foo
              class << self
                def bar; end
              end
            end
            ",
            "
            EnterClass(Foo)
              EnterSingletonClass(Foo::<Foo>)
                EnterMethod(bar())
                ExitScope
              ExitScope
            ExitScope
            ",
        );
    }

    // -- Method tests --

    #[test]
    fn build_def_node() {
        assert_operations(
            "
            def foo; end

            class Foo
              def bar; end
              def self.baz; end
            end
            ",
            "
            EnterMethod(foo())
            ExitScope
            EnterClass(Foo)
              EnterMethod(bar())
              ExitScope
              EnterMethod(self.baz())
              ExitScope
            ExitScope
            ",
        );
    }

    #[test]
    fn build_def_node_with_constant_receiver() {
        assert_operations(
            "
            class Bar
              def Foo.quz; end
            end
            ",
            "
            EnterClass(Bar)
              EnterMethod(Foo.quz())
              ExitScope
            ExitScope
            ",
        );
    }

    // -- Visibility tests --

    #[test]
    fn build_default_visibility() {
        assert_operations(
            "
            class Foo
              private

              def m1; end

              public

              def m2; end
            end
            ",
            "
            EnterClass(Foo)
              SetDefaultVisibility(private)
              EnterMethod(m1())
              ExitScope
              SetDefaultVisibility(public)
              EnterMethod(m2())
              ExitScope
            ExitScope
            ",
        );
    }

    #[test]
    fn build_inline_visibility() {
        assert_operations(
            "
            protected def m1; end
            ",
            "
            SetDefaultVisibility(protected)
            EnterMethod(m1())
            ExitScope
            SetDefaultVisibility(private)
            ",
        );
    }

    // TODO: `private :bar` with symbol args should produce SetMethodVisibility operations.
    // This is one of the key motivations for the operation-based approach.

    #[test]
    fn build_module_function() {
        assert_operations(
            "
            module Foo
              module_function

              def bar; end
            end
            ",
            "
            EnterModule(Foo)
              SetDefaultVisibility(module_function)
              EnterMethod(self.bar())
              ExitScope
              EnterMethod(bar())
              ExitScope
            ExitScope
            ",
        );
    }

    #[test]
    fn build_module_function_with_ivar() {
        assert_operations(
            "
            module Foo
              module_function

              def bar
                @x = 1
              end
            end
            ",
            "
            EnterModule(Foo)
              SetDefaultVisibility(module_function)
              EnterMethod(self.bar())
                DefineInstanceVariable(@x)
              ExitScope
              EnterMethod(bar())
                DefineInstanceVariable(@x)
              ExitScope
            ExitScope
            ",
        );
    }

    // -- Constant tests --

    #[test]
    fn build_constant_write() {
        assert_operations(
            "
            FOO = 1

            class Bar
              BAZ = 2
            end
            ",
            "
            DefineConstant(FOO)
            EnterClass(Bar)
              DefineConstant(BAZ)
            ExitScope
            ",
        );
    }

    #[test]
    fn build_constant_path_write() {
        assert_operations(
            "
            FOO::BAR = 1
            ",
            "
            DefineConstant(FOO::BAR)
            ",
        );
    }

    #[test]
    fn build_constant_alias() {
        assert_operations(
            "
            ALIAS = OtherConstant
            ",
            "
            AliasConstant(ALIAS -> OtherConstant)
            ",
        );
    }

    #[test]
    fn build_set_constant_visibility() {
        assert_operations(
            "
            module Foo
              BAR = 42
              private_constant :BAR
            end
            ",
            "
            EnterModule(Foo)
              DefineConstant(BAR)
              SetConstantVisibility(BAR, vis: private)
            ExitScope
            ",
        );
    }

    #[test]
    fn build_public_constant() {
        assert_operations(
            "
            module Foo
              BAR = 42
              public_constant :BAR
            end
            ",
            "
            EnterModule(Foo)
              DefineConstant(BAR)
              SetConstantVisibility(BAR, vis: public)
            ExitScope
            ",
        );
    }

    #[test]
    fn build_private_constant_multiple() {
        assert_operations(
            "
            module Foo
              BAR = 42
              BAZ = 43
              private_constant :BAR, :BAZ
            end
            ",
            "
            EnterModule(Foo)
              DefineConstant(BAR)
              DefineConstant(BAZ)
              SetConstantVisibility(BAR, vis: private)
              SetConstantVisibility(BAZ, vis: private)
            ExitScope
            ",
        );
    }

    // -- Attribute tests --

    #[test]
    fn build_attr_accessor() {
        assert_operations(
            "
            class Foo
              attr_accessor :bar
              attr_reader :baz
              attr_writer :qux
            end
            ",
            "
            EnterClass(Foo)
              DefineAttribute(accessor bar())
              DefineAttribute(reader baz())
              DefineAttribute(writer qux())
            ExitScope
            ",
        );
    }

    #[test]
    fn build_multiple_attr_accessors() {
        assert_operations(
            "
            class Foo
              attr_accessor :bar, :baz
            end
            ",
            "
            EnterClass(Foo)
              DefineAttribute(accessor bar())
              DefineAttribute(accessor baz())
            ExitScope
            ",
        );
    }

    #[test]
    fn build_attr_with_visibility() {
        assert_operations(
            "
            class Foo
              private

              attr_reader :bar
            end
            ",
            "
            EnterClass(Foo)
              SetDefaultVisibility(private)
              DefineAttribute(reader bar())
            ExitScope
            ",
        );
    }

    // -- Mixin tests --

    #[test]
    fn build_mixins() {
        assert_operations(
            "
            class Foo
              include Bar
              prepend Baz
              extend Qux
            end
            ",
            "
            EnterClass(Foo)
              Mixin(include, Bar)
              Mixin(prepend, Baz)
              Mixin(extend, Qux)
            ExitScope
            ",
        );
    }

    // -- Alias tests --

    #[test]
    fn build_alias_method() {
        assert_operations(
            "
            class Foo
              alias foo bar
            end
            ",
            "
            EnterClass(Foo)
              AliasMethod(foo() -> bar())
            ExitScope
            ",
        );
    }

    #[test]
    fn build_alias_method_call() {
        assert_operations(
            "
            class Foo
              alias_method :new_name, :old_name
            end
            ",
            "
            EnterClass(Foo)
              AliasMethod(new_name() -> old_name())
            ExitScope
            ",
        );
    }

    #[test]
    fn build_alias_global_variable() {
        assert_operations(
            "
            alias $new $old
            ",
            "
            AliasGlobalVariable($new -> $old)
            ",
        );
    }

    // -- Variable tests --

    #[test]
    fn build_instance_variable() {
        assert_operations(
            "
            class Foo
              def initialize
                @bar = 1
              end
            end
            ",
            "
            EnterClass(Foo)
              EnterMethod(initialize())
                DefineInstanceVariable(@bar)
              ExitScope
            ExitScope
            ",
        );
    }

    #[test]
    fn build_class_variable() {
        assert_operations(
            "
            class Foo
              @@bar = 1
            end
            ",
            "
            EnterClass(Foo)
              DefineClassVariable(@@bar)
            ExitScope
            ",
        );
    }

    #[test]
    fn build_global_variable() {
        assert_operations(
            "
            $foo = 1
            ",
            "
            DefineGlobalVariable($foo)
            ",
        );
    }

    // -- Reference tests --

    #[test]
    fn build_constant_references() {
        assert_operations_with_references(
            "
            Foo
            ",
            "
            ReferenceConstant(Foo)
            ",
        );
    }

    #[test]
    fn build_method_references() {
        assert_operations_with_references(
            "
            foo
            ",
            "
            ReferenceMethod(foo)
            ",
        );
    }

    // -- Ordering tests --

    #[test]
    fn build_operations_ordering_with_visibility() {
        assert_operations(
            "
            class Foo
              def m1; end
              private
              def m2; end
            end
            ",
            "
            EnterClass(Foo)
              EnterMethod(m1())
              ExitScope
              SetDefaultVisibility(private)
              EnterMethod(m2())
              ExitScope
            ExitScope
            ",
        );
    }

    #[test]
    fn build_visibility_resets_in_nested_class() {
        assert_operations(
            "
            class Foo
              private

              class Bar
                def m1; end
              end

              def m2; end
            end
            ",
            "
            EnterClass(Foo)
              SetDefaultVisibility(private)
              EnterClass(Bar)
                EnterMethod(m1())
                ExitScope
              ExitScope
              EnterMethod(m2())
              ExitScope
            ExitScope
            ",
        );
    }

    #[test]
    fn build_visibility_in_singleton_class() {
        assert_operations(
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
            ",
            "
            EnterClass(Foo)
              SetDefaultVisibility(protected)
              EnterSingletonClass(Foo::<Foo>)
                EnterMethod(m1())
                ExitScope
                SetDefaultVisibility(private)
                EnterMethod(m2())
                ExitScope
              ExitScope
              EnterMethod(m3())
              ExitScope
            ExitScope
            ",
        );
    }

    #[test]
    fn build_top_level_method_visibility() {
        assert_operations(
            "
            def m1; end

            protected def m2; end

            public

            def m3; end
            ",
            "
            EnterMethod(m1())
            ExitScope
            SetDefaultVisibility(protected)
            EnterMethod(m2())
            ExitScope
            SetDefaultVisibility(private)
            SetDefaultVisibility(public)
            EnterMethod(m3())
            ExitScope
            ",
        );
    }
}
