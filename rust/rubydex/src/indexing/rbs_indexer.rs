//! Visit the RBS AST and create type definitions.

use core::panic;

use ruby_rbs::node::{
    self, AliasKind, ClassNode, CommentNode, ConstantNode, ExtendNode, FunctionTypeNode, GlobalNode, IncludeNode,
    ModuleNode, Node, NodeList, PrependNode, TypeNameNode, Visit,
};

use crate::diagnostic::Rule;
use crate::indexing::local_graph::LocalGraph;
use crate::model::comment::Comment;
use crate::model::definitions::{
    ClassDefinition, ConstantDefinition, Definition, DefinitionFlags, ExtendDefinition, GlobalVariableDefinition,
    IncludeDefinition, MethodAliasDefinition, MethodDefinition, Mixin, ModuleDefinition, Parameter, ParameterStruct,
    PrependDefinition, Receiver, Signature, Signatures,
};
use crate::model::document::Document;
use crate::model::ids::{ConstantReferenceId, DefinitionId, NameId, StringId, UriId};
use crate::model::name::{Name, ParentScope};
use crate::model::references::ConstantReference;
use crate::model::visibility::Visibility;
use crate::offset::Offset;

pub struct RBSIndexer<'a> {
    uri_id: UriId,
    local_graph: LocalGraph,
    source: &'a str,
    nesting_stack: Vec<DefinitionId>,
    current_visibility: Visibility,
}

impl<'a> RBSIndexer<'a> {
    #[must_use]
    pub fn new(uri: String, source: &'a str) -> Self {
        let uri_id = UriId::from(&uri);
        let local_graph = LocalGraph::new(uri_id, Document::new(uri, source));

        Self {
            uri_id,
            local_graph,
            source,
            nesting_stack: Vec::new(),
            current_visibility: Visibility::Public,
        }
    }

    #[must_use]
    pub fn local_graph(self) -> LocalGraph {
        self.local_graph
    }

    pub fn index(&mut self) {
        let Ok(signature) = node::parse(self.source) else {
            self.local_graph.add_diagnostic(
                Rule::ParseError,
                Offset::new(0, 0),
                "Failed to parse RBS document".to_string(),
            );
            return;
        };

        self.visit(&signature.as_node());
    }

    /// Converts an RBS `TypeNameNode` into a rubydex `NameId`.
    ///
    /// Walks the namespace path (e.g. `Foo::Bar` in `Foo::Bar::Baz`) to build
    /// a `ParentScope` chain, then creates the final `Name` for the leaf segment.
    fn index_type_name(&mut self, type_name: &TypeNameNode, nesting_name_id: Option<NameId>) -> NameId {
        let namespace = type_name.namespace();

        let mut parent_scope = if namespace.absolute() {
            ParentScope::TopLevel
        } else {
            ParentScope::None
        };

        for path_node in namespace.path().iter() {
            let Node::Symbol(symbol) = path_node else {
                continue;
            };
            parent_scope = ParentScope::Some(self.intern_name(&symbol, parent_scope, nesting_name_id));
        }

        self.intern_name(&type_name.name(), parent_scope, nesting_name_id)
    }

    fn intern_name(
        &mut self,
        symbol: &node::SymbolNode,
        parent_scope: ParentScope,
        nesting_name_id: Option<NameId>,
    ) -> NameId {
        let string_id = self.local_graph.intern_string(symbol.as_str().to_owned());
        self.local_graph
            .add_name(Name::new(string_id, parent_scope, nesting_name_id))
    }

    fn parent_lexical_scope_id(&self) -> Option<DefinitionId> {
        self.nesting_stack.last().copied()
    }

    fn nesting_name_id(&self, lexical_nesting_id: Option<DefinitionId>) -> Option<NameId> {
        lexical_nesting_id.map(|id| {
            let owner = self
                .local_graph
                .definitions()
                .get(&id)
                .expect("owner definition should exist");
            *owner.name_id().expect("nesting definition should have a name")
        })
    }

    fn add_mixin_to_current_lexical_scope(&mut self, owner_id: DefinitionId, mixin: Mixin) {
        let owner = self
            .local_graph
            .get_definition_mut(owner_id)
            .expect("owner definition should exist");

        match owner {
            Definition::Class(class) => class.add_mixin(mixin),
            Definition::Module(module) => module.add_mixin(mixin),
            _ => unreachable!("RBS nesting stack only contains modules/classes"),
        }
    }

    fn index_mixin(&mut self, type_name: &TypeNameNode, mixin_fn: fn(ConstantReferenceId) -> Mixin) {
        let Some(lexical_nesting_id) = self.parent_lexical_scope_id() else {
            return;
        };

        let nesting_name_id = self.nesting_name_id(Some(lexical_nesting_id));
        let name_id = self.index_type_name(type_name, nesting_name_id);
        let offset = Offset::from_rbs_location(&type_name.location());

        let constant_ref_id =
            self.local_graph
                .add_constant_reference(ConstantReference::new(name_id, self.uri_id, offset));

        self.add_mixin_to_current_lexical_scope(lexical_nesting_id, mixin_fn(constant_ref_id));
    }

    fn add_member_to_current_lexical_scope(&mut self, owner_id: DefinitionId, member_id: DefinitionId) {
        let owner = self
            .local_graph
            .get_definition_mut(owner_id)
            .expect("owner definition should exist");

        match owner {
            Definition::Module(module) => module.add_member(member_id),
            Definition::Class(class) => class.add_member(member_id),
            _ => unreachable!("RBS nesting stack only contains modules/classes"),
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    fn collect_comments(&self, comment_node: Option<CommentNode>) -> Box<[Comment]> {
        let Some(comment_node) = comment_node else {
            return Box::new([]);
        };

        let location = comment_node.location();
        let start = location.start().cast_unsigned() as usize;
        let end = location.end().cast_unsigned() as usize;

        let comment_block = &self.source[start..end];
        let lines: Vec<&str> = comment_block.split('\n').collect();

        let mut comments = Vec::with_capacity(lines.len());
        let mut current_offset = start as u32;

        for (i, line) in lines.iter().enumerate() {
            let mut size = 1;
            let mut line = *line;

            if line.ends_with('\r') {
                line = line.trim_end_matches('\r');
                size += 1;
            }

            let line_indent = if i == 0 {
                0
            } else {
                line.len() - line.trim_start().len()
            };

            // Skip past indentation to the comment text
            current_offset += line_indent as u32;

            let line_text = line[line_indent..].to_string();
            let line_bytes = line_text.len() as u32;
            let offset = Offset::new(current_offset, current_offset + line_bytes);
            comments.push(Comment::new(offset, line_text));

            // Advance past current line text + \r (if present) + \n for the next line
            if i < lines.len() - 1 {
                current_offset += line_bytes + size;
            }
        }

        comments.into_boxed_slice()
    }

    fn register_definition(
        &mut self,
        definition: Definition,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> DefinitionId {
        let definition_id = self.local_graph.add_definition(definition);
        if let Some(id) = lexical_nesting_id {
            self.add_member_to_current_lexical_scope(id, definition_id);
        }
        definition_id
    }

    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    fn source_at(&self, location: &node::RBSLocationRange) -> String {
        let start = location.start() as usize;
        let end = location.end() as usize;
        self.source[start..end].to_string()
    }

    fn intern_param(&mut self, param: &node::FunctionParamNode, default_name: &str) -> ParameterStruct {
        if let Some(name_loc) = param.name_location() {
            let str_id = self.local_graph.intern_string(self.source_at(&name_loc));
            ParameterStruct::new(Offset::from_rbs_location(&name_loc), str_id)
        } else {
            let location = param.type_().location();
            let str_id = self.local_graph.intern_string(default_name.to_string());
            ParameterStruct::new(Offset::from_rbs_location(&location), str_id)
        }
    }

    fn collect_parameters(&mut self, function_node: &FunctionTypeNode) -> Vec<Parameter> {
        let mut parameters = Vec::new();
        let mut positional_index: usize = 0;

        for node in function_node.required_positionals().iter() {
            let Node::FunctionParam(param) = node else {
                panic!("Expected FunctionParam node, found {node:?}")
            };
            let default_name = format!("arg{positional_index}");
            parameters.push(Parameter::RequiredPositional(self.intern_param(&param, &default_name)));
            positional_index += 1;
        }

        for node in function_node.optional_positionals().iter() {
            let Node::FunctionParam(param) = node else {
                panic!("Expected FunctionParam node, found {node:?}")
            };
            let default_name = format!("arg{positional_index}");
            parameters.push(Parameter::OptionalPositional(self.intern_param(&param, &default_name)));
            positional_index += 1;
        }

        if let Some(node) = function_node.rest_positionals() {
            let Node::FunctionParam(param) = node else {
                panic!("Expected FunctionParam node, found {node:?}")
            };
            parameters.push(Parameter::RestPositional(self.intern_param(&param, "args")));
        }

        for node in function_node.trailing_positionals().iter() {
            let Node::FunctionParam(param) = node else {
                panic!("Expected FunctionParam node, found {node:?}")
            };
            let default_name = format!("arg{positional_index}");
            parameters.push(Parameter::Post(self.intern_param(&param, &default_name)));
            positional_index += 1;
        }

        for (key, _value) in function_node.required_keywords().iter() {
            let name = self.source_at(&key.location());
            let offset = Offset::from_rbs_location(&key.location());
            let str_id = self.local_graph.intern_string(name);
            parameters.push(Parameter::RequiredKeyword(ParameterStruct::new(offset, str_id)));
        }

        for (key, _value) in function_node.optional_keywords().iter() {
            let name = self.source_at(&key.location());
            let offset = Offset::from_rbs_location(&key.location());
            let str_id = self.local_graph.intern_string(name);
            parameters.push(Parameter::OptionalKeyword(ParameterStruct::new(offset, str_id)));
        }

        if let Some(node) = function_node.rest_keywords() {
            let Node::FunctionParam(param) = &node else {
                panic!("Expected FunctionParam node, found {node:?}")
            };
            parameters.push(Parameter::RestKeyword(self.intern_param(param, "kwargs")));
        }

        parameters
    }

    fn build_signatures(sigs: &mut Vec<Signature>) -> Signatures {
        match sigs.len() {
            0 => Signatures::Simple(Box::new([])),
            1 => Signatures::Simple(sigs.pop().unwrap()),
            _ => Signatures::Overloaded(std::mem::take(sigs).into_boxed_slice()),
        }
    }

    fn collect_overload_signatures(&mut self, def_node: &node::MethodDefinitionNode) -> Signatures {
        let mut sigs: Vec<Signature> = Vec::new();

        for overload_node in def_node.overloads().iter() {
            let Node::MethodDefinitionOverload(overload) = overload_node else {
                panic!("Expected MethodDefinitionOverload node in overloads, found {overload_node:?}");
            };
            let Node::MethodType(method_type) = overload.method_type() else {
                panic!(
                    "Expected MethodType node in overloads, found {:?}",
                    overload.method_type()
                );
            };
            let mut params = match method_type.type_() {
                Node::FunctionType(function_type) => self.collect_parameters(&function_type),
                Node::UntypedFunctionType(_) => Vec::new(),
                other => panic!("Expected FunctionType node in overloads, found {other:?}"),
            };
            if let Some(block) = method_type.block() {
                let str_id = self.local_graph.intern_string("block".to_string());
                let offset = Offset::from_rbs_location(&block.location());
                params.push(Parameter::Block(ParameterStruct::new(offset, str_id)));
            }
            sigs.push(params.into_boxed_slice());
        }

        Self::build_signatures(&mut sigs)
    }

    /// Registers two method definitions for `module_function` (SingletonInstance):
    /// a public singleton method and a private instance method.
    #[allow(clippy::too_many_arguments)]
    fn register_singleton_instance_method(
        &mut self,
        str_id: StringId,
        offset: Offset,
        comments: Box<[Comment]>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
        signatures: Signatures,
    ) {
        let singleton_def = Definition::Method(Box::new(MethodDefinition::new(
            str_id,
            self.uri_id,
            offset.clone(),
            comments.clone(),
            flags.clone(),
            lexical_nesting_id,
            signatures.clone(),
            Visibility::Public,
            lexical_nesting_id.map(Receiver::SelfReceiver),
        )));
        self.register_definition(singleton_def, lexical_nesting_id);

        let instance_def = Definition::Method(Box::new(MethodDefinition::new(
            str_id,
            self.uri_id,
            offset,
            comments,
            flags,
            lexical_nesting_id,
            signatures,
            Visibility::Private,
            None,
        )));
        self.register_definition(instance_def, lexical_nesting_id);
    }

    /// Extracts definition flags from the list of RBS annotations.
    ///
    /// panics when a non-annotation node is encountered in the list, since only annotations should be present.
    fn flags(list: &NodeList) -> DefinitionFlags {
        let mut flags = DefinitionFlags::empty();

        for node in list.iter() {
            if let Node::Annotation(annotation) = node {
                let string = annotation.string();
                let content = string.as_bytes();
                if content == b"deprecated" || content.starts_with(b"deprecated:") {
                    flags |= DefinitionFlags::DEPRECATED;
                }
            } else {
                panic!("Expected annotation node, found {node:?}");
            }
        }

        flags
    }
}

impl Visit for RBSIndexer<'_> {
    fn visit_class_node(&mut self, class_node: &ClassNode) {
        let lexical_nesting_id = self.parent_lexical_scope_id();
        let nesting_name_id = self.nesting_name_id(lexical_nesting_id);

        let type_name = class_node.name();
        let name_id = self.index_type_name(&type_name, nesting_name_id);
        let offset = Offset::from_rbs_location(&class_node.location());
        let name_offset = Offset::from_rbs_location(&type_name.name().location());

        let comments = self.collect_comments(class_node.comment());

        let superclass_ref = class_node.super_class().as_ref().map(|super_node| {
            let type_name = super_node.name();
            let name_id = self.index_type_name(&type_name, nesting_name_id);
            let offset = Offset::from_rbs_location(&super_node.location());
            self.local_graph
                .add_constant_reference(ConstantReference::new(name_id, self.uri_id, offset))
        });

        let definition = Definition::Class(Box::new(ClassDefinition::new(
            name_id,
            self.uri_id,
            offset,
            name_offset,
            comments,
            Self::flags(&class_node.annotations()),
            lexical_nesting_id,
            superclass_ref,
        )));

        let definition_id = self.register_definition(definition, lexical_nesting_id);
        self.nesting_stack.push(definition_id);
        let saved_visibility = std::mem::replace(&mut self.current_visibility, Visibility::Public);

        for member in class_node.members().iter() {
            self.visit(&member);
        }

        self.current_visibility = saved_visibility;
        self.nesting_stack.pop();
    }

    fn visit_module_node(&mut self, module_node: &ModuleNode) {
        let lexical_nesting_id = self.parent_lexical_scope_id();
        let nesting_name_id = self.nesting_name_id(lexical_nesting_id);

        let type_name = module_node.name();
        let name_id = self.index_type_name(&type_name, nesting_name_id);
        let offset = Offset::from_rbs_location(&module_node.location());
        let name_offset = Offset::from_rbs_location(&type_name.name().location());

        let comments = self.collect_comments(module_node.comment());

        let definition = Definition::Module(Box::new(ModuleDefinition::new(
            name_id,
            self.uri_id,
            offset,
            name_offset,
            comments,
            Self::flags(&module_node.annotations()),
            lexical_nesting_id,
        )));

        let definition_id = self.register_definition(definition, lexical_nesting_id);
        self.nesting_stack.push(definition_id);
        let saved_visibility = std::mem::replace(&mut self.current_visibility, Visibility::Public);

        for member in module_node.members().iter() {
            self.visit(&member);
        }

        self.current_visibility = saved_visibility;
        self.nesting_stack.pop();
    }

    fn visit_constant_node(&mut self, constant_node: &ConstantNode) {
        let lexical_nesting_id = self.parent_lexical_scope_id();
        let nesting_name_id = self.nesting_name_id(lexical_nesting_id);

        let type_name = constant_node.name();
        let name_id = self.index_type_name(&type_name, nesting_name_id);
        let offset = Offset::from_rbs_location(&constant_node.location());

        let comments = self.collect_comments(constant_node.comment());

        let definition = Definition::Constant(Box::new(ConstantDefinition::new(
            name_id,
            self.uri_id,
            offset,
            comments,
            Self::flags(&constant_node.annotations()),
            lexical_nesting_id,
        )));

        self.register_definition(definition, lexical_nesting_id);
    }

    fn visit_global_node(&mut self, global_node: &GlobalNode) {
        let lexical_nesting_id = self.parent_lexical_scope_id();

        let str_id = self
            .local_graph
            .intern_string(global_node.name().to_string());
        let offset = Offset::from_rbs_location(&global_node.location());

        let comments = self.collect_comments(global_node.comment());

        let definition = Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
            str_id,
            self.uri_id,
            offset,
            comments,
            Self::flags(&global_node.annotations()),
            lexical_nesting_id,
        )));

        self.register_definition(definition, lexical_nesting_id);
    }

    fn visit_include_node(&mut self, include_node: &IncludeNode) {
        self.index_mixin(&include_node.name(), |ref_id| {
            Mixin::Include(IncludeDefinition::new(ref_id))
        });
    }

    fn visit_prepend_node(&mut self, prepend_node: &PrependNode) {
        self.index_mixin(&prepend_node.name(), |ref_id| {
            Mixin::Prepend(PrependDefinition::new(ref_id))
        });
    }

    fn visit_extend_node(&mut self, extend_node: &ExtendNode) {
        self.index_mixin(&extend_node.name(), |ref_id| {
            Mixin::Extend(ExtendDefinition::new(ref_id))
        });
    }

    fn visit_alias_node(&mut self, alias_node: &node::AliasNode) {
        let lexical_nesting_id = self.parent_lexical_scope_id();

        let receiver = match alias_node.kind() {
            AliasKind::Instance => None,
            AliasKind::Singleton => lexical_nesting_id.map(Receiver::SelfReceiver),
        };

        let new_name = alias_node.new_name();
        let old_name = alias_node.old_name();

        let new_name_str_id = self.local_graph.intern_string(format!("{new_name}()"));
        let old_name_str_id = self.local_graph.intern_string(format!("{old_name}()"));

        let offset = Offset::from_rbs_location(&alias_node.location());
        let comments = self.collect_comments(alias_node.comment());

        let definition = Definition::MethodAlias(Box::new(MethodAliasDefinition::new(
            new_name_str_id,
            old_name_str_id,
            self.uri_id,
            offset,
            comments,
            Self::flags(&alias_node.annotations()),
            lexical_nesting_id,
            receiver,
        )));

        self.register_definition(definition, lexical_nesting_id);
    }

    fn visit_method_definition_node(&mut self, def_node: &node::MethodDefinitionNode) {
        let str_id = self
            .local_graph
            .intern_string(format!("{}()", def_node.name()));
        let offset = Offset::from_rbs_location(&def_node.location());
        let comments = self.collect_comments(def_node.comment());
        let flags = Self::flags(&def_node.annotations());
        let lexical_nesting_id = self.parent_lexical_scope_id();
        let signatures = self.collect_overload_signatures(def_node);

        if def_node.kind() == node::MethodDefinitionKind::SingletonInstance {
            self.register_singleton_instance_method(str_id, offset, comments, flags, lexical_nesting_id, signatures);
            return;
        }

        let (visibility, receiver) = match def_node.kind() {
            node::MethodDefinitionKind::Instance => {
                let vis = match def_node.visibility() {
                    node::MethodDefinitionVisibility::Private => Visibility::Private,
                    node::MethodDefinitionVisibility::Public => Visibility::Public,
                    node::MethodDefinitionVisibility::Unspecified => self.current_visibility,
                };
                (vis, None)
            }
            node::MethodDefinitionKind::Singleton => {
                let vis = match def_node.visibility() {
                    node::MethodDefinitionVisibility::Private => Visibility::Private,
                    node::MethodDefinitionVisibility::Public | node::MethodDefinitionVisibility::Unspecified => {
                        Visibility::Public
                    }
                };
                (
                    vis,
                    Some(Receiver::SelfReceiver(
                        lexical_nesting_id.expect("Singleton method must have a lexical enclosing scope"),
                    )),
                )
            }
            node::MethodDefinitionKind::SingletonInstance => unreachable!("handled above"),
        };

        let definition = Definition::Method(Box::new(MethodDefinition::new(
            str_id,
            self.uri_id,
            offset,
            comments,
            flags,
            lexical_nesting_id,
            signatures,
            visibility,
            receiver,
        )));

        self.register_definition(definition, lexical_nesting_id);
    }

    fn visit_public_node(&mut self, _public_node: &node::PublicNode) {
        self.current_visibility = Visibility::Public;
    }

    fn visit_private_node(&mut self, _private_node: &node::PrivateNode) {
        self.current_visibility = Visibility::Private;
    }
}

#[cfg(test)]
mod tests {
    use ruby_rbs::node::{self, Node, NodeList};

    use crate::indexing::rbs_indexer::RBSIndexer;
    use crate::model::definitions::{Definition, DefinitionFlags, Parameter, Signatures};
    use crate::model::visibility::Visibility;
    use crate::test_utils::LocalGraphTest;
    use crate::{
        assert_def_comments_eq, assert_def_mixins_eq, assert_def_name_eq, assert_def_name_offset_eq, assert_def_str_eq,
        assert_def_superclass_ref_eq, assert_definition_at, assert_local_diagnostics_eq, assert_method_has_receiver,
        assert_no_local_diagnostics, assert_offset_string, assert_string_eq,
    };

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

    fn index_source(source: &str) -> LocalGraphTest {
        LocalGraphTest::new_rbs("file:///foo.rbs", source)
    }

    #[test]
    fn index_source_with_errors() {
        let context = index_source("module");

        assert_local_diagnostics_eq!(&context, ["parse-error: Failed to parse RBS document (1:1-1:1)"]);

        assert!(context.graph().definitions().is_empty());
    }

    #[test]
    fn index_module_node() {
        let context = index_source({
            "
            module Foo
              module Bar
              end
            end
            "
        });

        assert_no_local_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 2);

        assert_definition_at!(&context, "1:1-4:4", Module, |def| {
            assert_def_name_eq!(&context, def, "Foo");
            assert_def_name_offset_eq!(&context, def, "1:8-1:11");
            assert_eq!(1, def.members().len());
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:3-3:6", Module, |def| {
            assert_def_name_eq!(&context, def, "Bar");
            assert_def_name_offset_eq!(&context, def, "2:10-2:13");

            assert_definition_at!(&context, "1:1-4:4", Module, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });
    }

    #[test]
    fn index_module_node_with_qualified_name() {
        let context = index_source({
            "
            module Foo
              module Bar::Baz
              end
            end
            "
        });

        assert_no_local_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 2);

        assert_definition_at!(&context, "1:1-4:4", Module, |def| {
            assert_def_name_eq!(&context, def, "Foo");
            assert_def_name_offset_eq!(&context, def, "1:8-1:11");
            assert_eq!(1, def.members().len());
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:3-3:6", Module, |def| {
            assert_def_name_eq!(&context, def, "Bar::Baz");
            assert_def_name_offset_eq!(&context, def, "2:15-2:18");

            assert_definition_at!(&context, "1:1-4:4", Module, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });
    }

    #[test]
    fn index_class_node() {
        let context = index_source({
            "
            class Foo
              class Bar
              end
            end
            "
        });

        assert_no_local_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 2);

        assert_definition_at!(&context, "1:1-4:4", Class, |def| {
            assert_def_name_eq!(&context, def, "Foo");
            assert_def_name_offset_eq!(&context, def, "1:7-1:10");
            assert_eq!(1, def.members().len());
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "2:3-3:6", Class, |def| {
            assert_def_name_eq!(&context, def, "Bar");
            assert_def_name_offset_eq!(&context, def, "2:9-2:12");

            assert_definition_at!(&context, "1:1-4:4", Class, |parent_nesting| {
                assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(parent_nesting.members()[0], def.id());
            });
        });
    }

    #[test]
    fn index_class_node_with_superclass() {
        let context = index_source({
            "
            class Foo < Bar
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-2:4", Class, |def| {
            assert_def_name_eq!(&context, def, "Foo");
            assert_def_superclass_ref_eq!(&context, def, "Bar");
        });
    }

    #[test]
    fn index_constant_node() {
        let context = index_source("FOO: String");

        assert_no_local_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 1);

        assert_definition_at!(&context, "1:1-1:12", Constant, |def| {
            assert_def_name_eq!(&context, def, "FOO");
            assert!(def.lexical_nesting_id().is_none());
        });
    }

    #[test]
    fn index_qualified_constant_node() {
        let context = index_source("Foo::BAR: String");

        assert_no_local_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 1);

        assert_definition_at!(&context, "1:1-1:17", Constant, |def| {
            assert_def_name_eq!(&context, def, "Foo::BAR");
        });
    }

    #[test]
    fn index_constant_inside_class() {
        let context = index_source({
            "
            class Foo
              FOO: Integer
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-3:4", Class, |class_def| {
            assert_def_name_eq!(&context, class_def, "Foo");
            assert_eq!(1, class_def.members().len());

            assert_definition_at!(&context, "2:3-2:15", Constant, |def| {
                assert_def_name_eq!(&context, def, "FOO");
                assert_eq!(class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(class_def.members()[0], def.id());
            });
        });
    }

    #[test]
    fn index_constant_node_with_comment() {
        let context = index_source({
            "
            # Some documentation
            FOO: String
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "2:1-2:12", Constant, |def| {
            assert_def_name_eq!(&context, def, "FOO");
            assert_def_comments_eq!(&context, def, ["# Some documentation"]);
        });
    }

    #[test]
    fn index_global_node() {
        let context = index_source({
            "
            $foo: String

            # A global variable
            $bar: Integer
            "
        });

        assert_no_local_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 2);

        assert_definition_at!(&context, "1:1-1:13", GlobalVariable, |def| {
            assert_def_str_eq!(&context, def, "$foo");
            assert!(def.lexical_nesting_id().is_none());
        });

        assert_definition_at!(&context, "4:1-4:14", GlobalVariable, |def| {
            assert_def_str_eq!(&context, def, "$bar");
            assert_def_comments_eq!(&context, def, ["# A global variable"]);
        });
    }

    #[test]
    fn index_mixins() {
        let context = index_source({
            "
            class Foo
              include Bar
              prepend Baz
              extend Qux
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-5:4", Class, |def| {
            assert_def_mixins_eq!(&context, def, Include, ["Bar"]);
            assert_def_mixins_eq!(&context, def, Prepend, ["Baz"]);
            assert_def_mixins_eq!(&context, def, Extend, ["Qux"]);
        });
    }

    #[test]
    fn index_multiple_includes() {
        let context = index_source({
            "
            module Foo
              include Bar
              include Baz
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-4:4", Module, |def| {
            assert_def_mixins_eq!(&context, def, Include, ["Bar", "Baz"]);
        });
    }

    #[test]
    fn index_include_qualified_name() {
        let context = index_source({
            "
            class Foo
              include Bar::Baz
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-3:4", Class, |def| {
            assert_def_mixins_eq!(&context, def, Include, ["Baz"]);
        });
    }

    #[test]
    fn index_class_and_module_nesting() {
        let context = index_source({
            "
            module Foo
              class Bar
              end
            end
            "
        });

        assert_no_local_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 2);

        assert_definition_at!(&context, "1:1-4:4", Module, |module_def| {
            assert_def_name_eq!(&context, module_def, "Foo");
            assert_eq!(1, module_def.members().len());

            assert_definition_at!(&context, "2:3-3:6", Class, |class_def| {
                assert_eq!(module_def.members()[0], class_def.id());
                assert_def_name_eq!(&context, class_def, "Bar");
                assert_eq!(module_def.id(), class_def.lexical_nesting_id().unwrap());
            });
        });
    }

    #[test]
    fn flags_test_deprecation() {
        fn extract_annotations<F: FnOnce(&NodeList)>(annots: &[u8], f: F) {
            let source = format!("{} module Foo end", std::str::from_utf8(annots).unwrap());
            let Ok(signature) = node::parse(&source) else {
                panic!("Failed to parse RBS source");
            };
            let decl = signature
                .declarations()
                .iter()
                .next()
                .expect("Expected at least one declaration");
            let Node::Module(module_node) = decl else {
                panic!("Expected a module declaration");
            };
            f(&module_node.annotations());
        }

        extract_annotations(b"%a{deprecated}", |list| {
            assert!(RBSIndexer::flags(list).contains(DefinitionFlags::DEPRECATED));
        });
        extract_annotations(b"%a{deprecated: Some message here}", |list| {
            assert!(RBSIndexer::flags(list).contains(DefinitionFlags::DEPRECATED));
        });
        extract_annotations(b"%a{deprecated} %a{pure}", |list| {
            assert!(RBSIndexer::flags(list).contains(DefinitionFlags::DEPRECATED));
        });
        extract_annotations(b"", |list| {
            assert!(!RBSIndexer::flags(list).contains(DefinitionFlags::DEPRECATED));
        });
        extract_annotations(b"%a{deprecatedxxxx}", |list| {
            assert!(!RBSIndexer::flags(list).contains(DefinitionFlags::DEPRECATED));
        });
    }

    #[test]
    fn index_declarations_with_deprecation() {
        let context = index_source({
            "
            %a{deprecated}
            module Foo
            end

            %a{deprecated: Use Bar2 instead}
            class Bar
            end

            %a(deprecated)
            FOO: String

            %a[deprecated]
            $BAR: String
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "2:1-3:4", Module, |def| {
            assert_def_name_eq!(&context, def, "Foo");
            assert!(def.flags().contains(DefinitionFlags::DEPRECATED));
        });

        assert_definition_at!(&context, "6:1-7:4", Class, |def| {
            assert_def_name_eq!(&context, def, "Bar");
            assert!(def.flags().contains(DefinitionFlags::DEPRECATED));
        });

        assert_definition_at!(&context, "10:1-10:12", Constant, |def| {
            assert_def_name_eq!(&context, def, "FOO");
            assert!(def.flags().contains(DefinitionFlags::DEPRECATED));
        });

        assert_definition_at!(&context, "13:1-13:13", GlobalVariable, |def| {
            assert_def_str_eq!(&context, def, "$BAR");
            assert!(def.flags().contains(DefinitionFlags::DEPRECATED));
        });
    }

    #[test]
    fn index_alias_node() {
        let context = index_source({
            "
            class Foo
              # Some documentation
              alias bar baz
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-4:4", Class, |class_def| {
            assert_eq!(1, class_def.members().len());

            assert_definition_at!(&context, "3:3-3:16", MethodAlias, |def| {
                assert_string_eq!(&context, def.new_name_str_id(), "bar()");
                assert_string_eq!(&context, def.old_name_str_id(), "baz()");
                assert_def_comments_eq!(&context, def, ["# Some documentation"]);
                assert_eq!(class_def.id(), def.lexical_nesting_id().unwrap());
            });
        });
    }

    #[test]
    fn index_alias_node_with_deprecation() {
        let context = index_source({
            "
            class Foo
              %a{deprecated}
              alias bar baz
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "3:3-3:16", MethodAlias, |def| {
            assert!(def.flags().contains(DefinitionFlags::DEPRECATED));
        });
    }

    #[test]
    fn index_alias_node_singleton() {
        let context = index_source({
            "
            class Foo
              alias self.bar self.baz
            end
            "
        });

        assert_no_local_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 2);

        assert_definition_at!(&context, "2:3-2:26", MethodAlias, |def| {
            assert_string_eq!(&context, def.new_name_str_id(), "bar()");
            assert_string_eq!(&context, def.old_name_str_id(), "baz()");
            assert_method_has_receiver!(&context, def, "Foo");
        });
    }

    #[test]
    fn mixed_singleton_instance_alias_is_not_indexed() {
        // Mixed aliases (`alias self.x y` and `alias x self.y`) are not valid RBS.
        // Verify that no alias definitions are produced for these inputs.
        for source in [
            "
            class Foo
              alias self.bar baz
            end
            ",
            "
            class Foo
              alias bar self.baz
            end
            ",
        ] {
            let context = index_source(source);
            let has_alias = context
                .graph()
                .definitions()
                .values()
                .any(|d| matches!(d, Definition::MethodAlias(_)));
            assert!(!has_alias, "Expected no alias definitions for: {source}");
        }
    }

    #[test]
    fn split_multiline_comments() {
        let context = index_source({
            "
            # First line
            # Second line
            # Third line
            class Foo
              # A comment for Bar
                # Another line for Bar
            # One more line for Bar
              module Bar
              end
            end

            # splits strings at the \\n char
            BAZ: Integer
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "4:1-10:4", Class, |def| {
            assert_def_name_eq!(&context, def, "Foo");
            assert_def_comments_eq!(&context, def, ["# First line", "# Second line", "# Third line"]);
        });

        assert_definition_at!(&context, "8:3-9:6", Module, |def| {
            assert_def_name_eq!(&context, def, "Bar");
            assert_def_comments_eq!(
                &context,
                def,
                [
                    "# A comment for Bar",
                    "# Another line for Bar",
                    "# One more line for Bar"
                ]
            );
        });

        assert_definition_at!(&context, "13:1-13:13", Constant, |def| {
            assert_def_name_eq!(&context, def, "BAZ");
            assert_def_comments_eq!(&context, def, ["# splits strings at the \\n char"]);
        });
    }

    #[test]
    fn split_multiline_comments_crlf() {
        // Build the indexer directly to bypass normalize_indentation, which strips \r
        let source = "# First line\r\n# Second line\r\nclass Foo\r\nend\r\n";
        let mut indexer = RBSIndexer::new("file:///foo.rbs".to_string(), source);
        indexer.index();
        let context = LocalGraphTest::from_local_graph("file:///foo.rbs", source, indexer.local_graph());

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "3:1-4:4", Class, |def| {
            assert_def_name_eq!(&context, def, "Foo");
            assert_def_comments_eq!(&context, def, ["# First line", "# Second line"]);
        });
    }

    #[test]
    fn index_method_definition() {
        let context = index_source({
            "
            class Foo
              def foo: () -> void

              def bar: (?) -> void
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "1:1-5:4", Class, |class_def| {
            assert_def_name_eq!(&context, class_def, "Foo");
            assert_eq!(2, class_def.members().len());

            assert_definition_at!(&context, "2:3-2:22", Method, |def| {
                assert_def_str_eq!(&context, def, "foo()");
                assert!(def.receiver().is_none());
                assert_eq!(def.visibility(), &Visibility::Public);
                assert_eq!(class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(class_def.members()[0], def.id());
            });

            assert_definition_at!(&context, "4:3-4:23", Method, |def| {
                assert_def_str_eq!(&context, def, "bar()");
                assert!(def.receiver().is_none());
                assert_eq!(def.visibility(), &Visibility::Public);
                assert_eq!(class_def.id(), def.lexical_nesting_id().unwrap());
                assert_eq!(class_def.members()[1], def.id());
            });
        });
    }

    #[test]
    fn index_method_definition_with_parameters() {
        let context = index_source({
            "
            class Foo
              def foo: (String, ?Integer, *String, Symbol, name: String, ?age: Integer, **untyped) -> void

              def bar: (String a, ?Integer b, *String c, Symbol d, name: String e, ?age: Integer f, **untyped rest) -> void
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        // Method without parameter names
        assert_definition_at!(&context, "2:3-2:95", Method, |def| {
            assert_def_str_eq!(&context, def, "foo()");
            assert_eq!(def.signatures().as_slice()[0].len(), 7);

            assert_parameter!(&def.signatures().as_slice()[0][0], RequiredPositional, |param| {
                assert_string_eq!(context, param.str(), "arg0");
                assert_offset_string!(context, param.offset(), "String");
            });

            assert_parameter!(&def.signatures().as_slice()[0][1], OptionalPositional, |param| {
                assert_string_eq!(context, param.str(), "arg1");
                assert_offset_string!(context, param.offset(), "Integer");
            });

            assert_parameter!(&def.signatures().as_slice()[0][2], RestPositional, |param| {
                assert_string_eq!(context, param.str(), "args");
                assert_offset_string!(context, param.offset(), "String");
            });

            assert_parameter!(&def.signatures().as_slice()[0][3], Post, |param| {
                assert_string_eq!(context, param.str(), "arg2");
                assert_offset_string!(context, param.offset(), "Symbol");
            });

            assert_parameter!(&def.signatures().as_slice()[0][4], RequiredKeyword, |param| {
                assert_string_eq!(context, param.str(), "name");
                assert_offset_string!(context, param.offset(), "name");
            });

            assert_parameter!(&def.signatures().as_slice()[0][5], OptionalKeyword, |param| {
                assert_string_eq!(context, param.str(), "age");
                assert_offset_string!(context, param.offset(), "age");
            });

            assert_parameter!(&def.signatures().as_slice()[0][6], RestKeyword, |param| {
                assert_string_eq!(context, param.str(), "kwargs");
                assert_offset_string!(context, param.offset(), "untyped");
            });
        });

        // Method with parameter names
        assert_definition_at!(&context, "4:3-4:112", Method, |def| {
            assert_def_str_eq!(&context, def, "bar()");
            assert_eq!(def.signatures().as_slice()[0].len(), 7);

            assert_parameter!(&def.signatures().as_slice()[0][0], RequiredPositional, |param| {
                assert_string_eq!(context, param.str(), "a");
                assert_offset_string!(context, param.offset(), "a");
            });

            assert_parameter!(&def.signatures().as_slice()[0][1], OptionalPositional, |param| {
                assert_string_eq!(context, param.str(), "b");
                assert_offset_string!(context, param.offset(), "b");
            });

            assert_parameter!(&def.signatures().as_slice()[0][2], RestPositional, |param| {
                assert_string_eq!(context, param.str(), "c");
                assert_offset_string!(context, param.offset(), "c");
            });

            assert_parameter!(&def.signatures().as_slice()[0][3], Post, |param| {
                assert_string_eq!(context, param.str(), "d");
                assert_offset_string!(context, param.offset(), "d");
            });

            assert_parameter!(&def.signatures().as_slice()[0][4], RequiredKeyword, |param| {
                assert_string_eq!(context, param.str(), "name");
                assert_offset_string!(context, param.offset(), "name");
            });

            assert_parameter!(&def.signatures().as_slice()[0][5], OptionalKeyword, |param| {
                assert_string_eq!(context, param.str(), "age");
                assert_offset_string!(context, param.offset(), "age");
            });

            assert_parameter!(&def.signatures().as_slice()[0][6], RestKeyword, |param| {
                assert_string_eq!(context, param.str(), "rest");
                assert_offset_string!(context, param.offset(), "rest");
            });
        });
    }

    #[test]
    fn index_method_definition_with_multiple_overloads() {
        let context = index_source({
            "
            class Foo
              def foo: (String) -> Integer
                     | (Integer) -> String
                     | (Symbol, String) -> void
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "2:3-4:36", Method, |def| {
            assert_def_str_eq!(&context, def, "foo()");
            let sigs = def.signatures().as_slice();
            assert_eq!(sigs.len(), 3);

            // First overload: (String) -> Integer
            assert_eq!(sigs[0].len(), 1);
            assert_parameter!(&sigs[0][0], RequiredPositional, |param| {
                assert_string_eq!(context, param.str(), "arg0");
            });

            // Second overload: (Integer) -> String
            assert_eq!(sigs[1].len(), 1);
            assert_parameter!(&sigs[1][0], RequiredPositional, |param| {
                assert_string_eq!(context, param.str(), "arg0");
            });

            // Third overload: (Symbol, String) -> void
            assert_eq!(sigs[2].len(), 2);
            assert_parameter!(&sigs[2][0], RequiredPositional, |param| {
                assert_string_eq!(context, param.str(), "arg0");
            });
            assert_parameter!(&sigs[2][1], RequiredPositional, |param| {
                assert_string_eq!(context, param.str(), "arg1");
            });

            assert!(matches!(def.signatures(), Signatures::Overloaded(_)));
        });
    }

    #[test]
    fn index_method_definition_with_dot_dot_dot() {
        let context = index_source({
            "
            class Foo
              def to_s: ...
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "2:3-2:16", Method, |def| {
            assert_def_str_eq!(&context, def, "to_s()");
            let sigs = def.signatures().as_slice();
            assert_eq!(sigs.len(), 1);
            assert_eq!(sigs[0].len(), 0);
            assert!(matches!(def.signatures(), Signatures::Simple(_)));
        });
    }

    #[test]
    fn index_method_definition_module_function() {
        let context = index_source({
            "
            class Foo
              def self?.foo: () -> void
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        let definitions = context.all_definitions_at("2:3-2:28");
        assert_eq!(definitions.len(), 2, "module_function should create two definitions");

        let instance_method = definitions
            .iter()
            .find(|d| matches!(d, Definition::Method(m) if m.receiver().is_none()))
            .expect("should have instance method definition");
        let Definition::Method(instance_method) = instance_method else {
            panic!()
        };
        assert_def_str_eq!(&context, instance_method, "foo()");
        assert_eq!(instance_method.visibility(), &Visibility::Private);

        let singleton_method = definitions
            .iter()
            .find(|d| matches!(d, Definition::Method(m) if m.receiver().is_some()))
            .expect("should have singleton method definition");
        let Definition::Method(singleton_method) = singleton_method else {
            panic!()
        };
        assert_def_str_eq!(&context, singleton_method, "foo()");
        assert_eq!(singleton_method.visibility(), &Visibility::Public);
    }

    #[test]
    fn index_method_definition_singleton_method() {
        let context = index_source({
            "
            class Foo
              def self.foo: () -> void
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "2:3-2:27", Method, |def| {
            assert_def_str_eq!(&context, def, "foo()");
            let sigs = def.signatures().as_slice();
            assert_eq!(sigs.len(), 1);
            assert_eq!(sigs[0].len(), 0);
            assert!(matches!(def.signatures(), Signatures::Simple(_)));
            assert_method_has_receiver!(&context, def, "Foo");
        });
    }

    #[test]
    fn index_method_definition_public_private_method() {
        let context = index_source({
            "
            class Foo
              public def foo: () -> void

              private def bar: () -> void
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        assert_definition_at!(&context, "2:3-2:29", Method, |def| {
            assert_def_str_eq!(&context, def, "foo()");
            let sigs = def.signatures().as_slice();
            assert_eq!(sigs.len(), 1);
            assert_eq!(sigs[0].len(), 0);
            assert!(matches!(def.signatures(), Signatures::Simple(_)));
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        assert_definition_at!(&context, "4:3-4:30", Method, |def| {
            assert_def_str_eq!(&context, def, "bar()");
            let sigs = def.signatures().as_slice();
            assert_eq!(sigs.len(), 1);
            assert_eq!(sigs[0].len(), 0);
            assert!(matches!(def.signatures(), Signatures::Simple(_)));
            assert_eq!(def.visibility(), &Visibility::Private);
        });
    }

    #[test]
    fn index_method_definition_public_private_syntax() {
        let context = index_source({
            "
            class Foo
              def foo: () -> void
              def self.foo: () -> void

              public

              def bar: () -> void
              def self.bar: () -> void

              private

              def baz: () -> void
              def self.baz: () -> void
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        // Instance method: default visibility is public
        assert_definition_at!(&context, "2:3-2:22", Method, |def| {
            assert_def_str_eq!(&context, def, "foo()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        // Singleton method: always public regardless of current_visibility
        assert_definition_at!(&context, "3:3-3:27", Method, |def| {
            assert_def_str_eq!(&context, def, "foo()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        // Instance method: public due to `public` modifier on line 5
        assert_definition_at!(&context, "7:3-7:22", Method, |def| {
            assert_def_str_eq!(&context, def, "bar()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        // Singleton method: always public regardless of current_visibility
        assert_definition_at!(&context, "8:3-8:27", Method, |def| {
            assert_def_str_eq!(&context, def, "bar()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });

        // Instance method: private due to `private` modifier on line 10
        assert_definition_at!(&context, "12:3-12:22", Method, |def| {
            assert_def_str_eq!(&context, def, "baz()");
            assert_eq!(def.visibility(), &Visibility::Private);
        });

        // Singleton method: always public, ignores `private` modifier
        assert_definition_at!(&context, "13:3-13:27", Method, |def| {
            assert_def_str_eq!(&context, def, "baz()");
            assert_eq!(def.visibility(), &Visibility::Public);
        });
    }

    #[test]
    fn index_method_definition_with_block() {
        let context = index_source({
            "
            class Foo
              def foo: () { (String) -> void } -> void
            end
            "
        });

        assert_no_local_diagnostics!(&context);

        // Method with block: should have 1 parameter (Block)
        assert_definition_at!(&context, "2:3-2:43", Method, |def| {
            assert_def_str_eq!(&context, def, "foo()");
            assert_eq!(def.signatures().as_slice()[0].len(), 1);

            assert_parameter!(&def.signatures().as_slice()[0][0], Block, |param| {
                assert_string_eq!(context, param.str(), "block");
            });
        });
    }
}
