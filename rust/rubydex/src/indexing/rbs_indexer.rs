//! Visit the RBS AST and create type definitions.

use ruby_rbs::node::{
    self, ClassNode, CommentNode, ConstantNode, ExtendNode, GlobalNode, IncludeNode, ModuleNode, Node, PrependNode,
    TypeNameNode, Visit,
};

use crate::diagnostic::Rule;
use crate::indexing::local_graph::LocalGraph;
use crate::model::comment::Comment;
use crate::model::definitions::{
    ClassDefinition, ConstantDefinition, Definition, DefinitionFlags, ExtendDefinition, GlobalVariableDefinition,
    IncludeDefinition, Mixin, ModuleDefinition, PrependDefinition,
};
use crate::model::document::Document;
use crate::model::ids::{DefinitionId, NameId, ReferenceId, UriId};
use crate::model::name::{Name, ParentScope};
use crate::model::references::ConstantReference;
use crate::offset::Offset;

pub struct RBSIndexer<'a> {
    uri_id: UriId,
    local_graph: LocalGraph,
    source: &'a str,
    nesting_stack: Vec<DefinitionId>,
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
        }
    }

    #[must_use]
    pub fn local_graph(self) -> LocalGraph {
        self.local_graph
    }

    pub fn index(&mut self) {
        let Ok(signature) = node::parse(self.source.as_bytes()) else {
            self.local_graph.add_diagnostic(
                Rule::ParseError,
                Offset::new(0, 0),
                "Failed to parse RBS document".to_string(),
            );
            return;
        };

        self.visit(&signature.as_node());
    }

    fn bytes_to_string(name: &[u8]) -> String {
        String::from_utf8_lossy(name).into_owned()
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
            parent_scope = ParentScope::Some(self.intern_name(symbol.name(), parent_scope, nesting_name_id));
        }

        self.intern_name(type_name.name().name(), parent_scope, nesting_name_id)
    }

    fn intern_name(&mut self, name_bytes: &[u8], parent_scope: ParentScope, nesting_name_id: Option<NameId>) -> NameId {
        let string_id = self.local_graph.intern_string(Self::bytes_to_string(name_bytes));
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

    fn index_mixin(&mut self, type_name: &TypeNameNode, mixin_fn: fn(ReferenceId) -> Mixin) {
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

    fn collect_comments(comment_node: Option<CommentNode>) -> Vec<Comment> {
        comment_node
            .into_iter()
            .map(|comment| {
                let text = Self::bytes_to_string(comment.string().as_bytes());
                Comment::new(Offset::from_rbs_location(&comment.location()), text)
            })
            .collect()
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
}

impl Visit for RBSIndexer<'_> {
    fn visit_class_node(&mut self, class_node: &ClassNode) {
        let lexical_nesting_id = self.parent_lexical_scope_id();
        let nesting_name_id = self.nesting_name_id(lexical_nesting_id);

        let type_name = class_node.name();
        let name_id = self.index_type_name(&type_name, nesting_name_id);
        let offset = Offset::from_rbs_location(&class_node.location());
        let name_offset = Offset::from_rbs_location(&type_name.name().location());

        let comments = Self::collect_comments(class_node.comment());

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
            DefinitionFlags::empty(),
            lexical_nesting_id,
            superclass_ref,
        )));

        let definition_id = self.register_definition(definition, lexical_nesting_id);
        self.nesting_stack.push(definition_id);

        for member in class_node.members().iter() {
            self.visit(&member);
        }

        self.nesting_stack.pop();
    }

    fn visit_module_node(&mut self, module_node: &ModuleNode) {
        let lexical_nesting_id = self.parent_lexical_scope_id();
        let nesting_name_id = self.nesting_name_id(lexical_nesting_id);

        let type_name = module_node.name();
        let name_id = self.index_type_name(&type_name, nesting_name_id);
        let offset = Offset::from_rbs_location(&module_node.location());
        let name_offset = Offset::from_rbs_location(&type_name.name().location());

        let comments = Self::collect_comments(module_node.comment());

        let definition = Definition::Module(Box::new(ModuleDefinition::new(
            name_id,
            self.uri_id,
            offset,
            name_offset,
            comments,
            DefinitionFlags::empty(),
            lexical_nesting_id,
        )));

        let definition_id = self.register_definition(definition, lexical_nesting_id);
        self.nesting_stack.push(definition_id);

        for member in module_node.members().iter() {
            self.visit(&member);
        }

        self.nesting_stack.pop();
    }

    fn visit_constant_node(&mut self, constant_node: &ConstantNode) {
        let lexical_nesting_id = self.parent_lexical_scope_id();
        let nesting_name_id = self.nesting_name_id(lexical_nesting_id);

        let type_name = constant_node.name();
        let name_id = self.index_type_name(&type_name, nesting_name_id);
        let offset = Offset::from_rbs_location(&constant_node.location());

        let comments = Self::collect_comments(constant_node.comment());

        let definition = Definition::Constant(Box::new(ConstantDefinition::new(
            name_id,
            self.uri_id,
            offset,
            comments,
            DefinitionFlags::empty(),
            lexical_nesting_id,
        )));

        self.register_definition(definition, lexical_nesting_id);
    }

    fn visit_global_node(&mut self, global_node: &GlobalNode) {
        let lexical_nesting_id = self.parent_lexical_scope_id();

        let str_id = self
            .local_graph
            .intern_string(Self::bytes_to_string(global_node.name().name()));
        let offset = Offset::from_rbs_location(&global_node.location());

        let comments = Self::collect_comments(global_node.comment());

        let definition = Definition::GlobalVariable(Box::new(GlobalVariableDefinition::new(
            str_id,
            self.uri_id,
            offset,
            comments,
            DefinitionFlags::empty(),
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
}

#[cfg(test)]
mod tests {
    use crate::test_utils::LocalGraphTest;
    use crate::{
        assert_def_comments_eq, assert_def_mixins_eq, assert_def_name_eq, assert_def_name_offset_eq, assert_def_str_eq,
        assert_def_superclass_ref_eq, assert_definition_at, assert_local_diagnostics_eq, assert_no_local_diagnostics,
    };

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
            assert!(def.superclass_ref().is_some());
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
            assert_def_comments_eq!(&context, def, ["Some documentation\n"]);
        });
    }

    #[test]
    fn index_global_node() {
        let context = index_source("$foo: String");

        assert_no_local_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 1);

        assert_definition_at!(&context, "1:1-1:13", GlobalVariable, |def| {
            assert_def_str_eq!(&context, def, "$foo");
            assert!(def.lexical_nesting_id().is_none());
        });
    }

    #[test]
    fn index_global_node_with_comment() {
        let context = index_source({
            "
            # A global variable
            $bar: Integer
            "
        });

        assert_no_local_diagnostics!(&context);
        assert_eq!(context.graph().definitions().len(), 1);

        assert_definition_at!(&context, "2:1-2:14", GlobalVariable, |def| {
            assert_def_str_eq!(&context, def, "$bar");
            assert_def_comments_eq!(&context, def, ["A global variable\n"]);
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
}
