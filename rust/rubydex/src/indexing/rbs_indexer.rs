//! Visit the RBS AST and create type definitions.

use ruby_rbs::node::{self, ModuleNode, Node, TypeNameNode, Visit};

use crate::diagnostic::Rule;
use crate::indexing::local_graph::LocalGraph;
use crate::model::comment::Comment;
use crate::model::definitions::{Definition, DefinitionFlags, ModuleDefinition};
use crate::model::document::Document;
use crate::model::ids::{DefinitionId, NameId, UriId};
use crate::model::name::{Name, ParentScope};
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
}

impl Visit for RBSIndexer<'_> {
    fn visit_module_node(&mut self, module_node: &ModuleNode) {
        let lexical_nesting_id = self.parent_lexical_scope_id();
        let nesting_name_id = lexical_nesting_id.map(|id| {
            let owner = self
                .local_graph
                .definitions()
                .get(&id)
                .expect("owner definition should exist");
            *owner.name_id().expect("nesting definition should have a name")
        });

        let type_name = module_node.name();
        let name_id = self.index_type_name(&type_name, nesting_name_id);
        let offset = Offset::from_rbs_location(&module_node.location());
        let name_offset = Offset::from_rbs_location(&type_name.name().location());

        let comments: Vec<_> = module_node
            .comment()
            .into_iter()
            .map(|comment| {
                let text = Self::bytes_to_string(comment.string().as_bytes());
                Comment::new(Offset::from_rbs_location(&comment.location()), text)
            })
            .collect();

        let definition = Definition::Module(Box::new(ModuleDefinition::new(
            name_id,
            self.uri_id,
            offset,
            name_offset,
            comments,
            DefinitionFlags::empty(),
            lexical_nesting_id,
        )));

        let definition_id = self.local_graph.add_definition(definition);
        if let Some(id) = lexical_nesting_id {
            self.add_member_to_current_lexical_scope(id, definition_id);
        }

        self.nesting_stack.push(definition_id);

        for member in module_node.members().iter() {
            self.visit(&member);
        }

        self.nesting_stack.pop();
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::LocalGraphTest;
    use crate::{assert_def_name_eq, assert_def_name_offset_eq, assert_definition_at};

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
        LocalGraphTest::new_rbs("file:///foo.rbs", source)
    }

    #[test]
    fn index_source_with_errors() {
        let context = index_source("module");

        assert_diagnostics_eq!(&context, ["parse-error: Failed to parse RBS document (1:1-1:1)"]);

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

        assert_no_diagnostics!(&context);
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

        assert_no_diagnostics!(&context);
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
}
