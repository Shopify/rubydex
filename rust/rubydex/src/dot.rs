use std::collections::{HashSet, VecDeque};
use std::fmt::Write;

use crate::model::{
    built_in,
    declaration::Declaration,
    definitions::{Definition, Mixin},
    document::Document,
    graph::Graph,
    ids::{DeclarationId, DefinitionId, UriId},
};

const DOC_COLOR: &str = "#4a90d9";
const DOC_FILL: &str = "#dce8f5";
const DEF_COLOR: &str = "#e8912d";
const DEF_FILL: &str = "#fdf0e0";
const DECL_COLOR: &str = "#5ba55b";
const DECL_FILL: &str = "#e0f0e0";
const NESTS_COLOR: &str = "#f0c08a";
const MEMBER_COLOR: &str = "#a3d9a3";
const SUPERCLASS_COLOR: &str = "#d94a7a";
const MIXIN_COLOR: &str = "#8b5fc7";

pub struct DotBuilder<'a> {
    output: String,
    graph: &'a Graph,
}

impl<'a> DotBuilder<'a> {
    fn new(graph: &'a Graph) -> Self {
        Self {
            output: String::new(),
            graph,
        }
    }

    fn graph(&self) -> &'a Graph {
        self.graph
    }

    fn writeln(&mut self, s: &str) {
        self.output.push_str(s);
        self.output.push('\n');
    }

    fn html_escape(s: &str) -> String {
        s.replace('&', "&amp;").replace('<', "&lt;").replace('>', "&gt;")
    }

    fn label(type_name: &str, name: &str, color: &str) -> String {
        let escaped = Self::html_escape(name);
        format!(
            concat!(
                "<<table border=\"0\" cellborder=\"0\" cellspacing=\"0\" align=\"center\">",
                "<tr><td align=\"center\"><font point-size=\"8\" color=\"{}\">{}</font></td></tr>",
                "<tr><td align=\"center\"><b>{}</b></td></tr>",
                "</table>>",
            ),
            color, type_name, escaped,
        )
    }

    #[must_use]
    pub fn generate(graph: &'a Graph, show_builtins: bool) -> String {
        let mut builder = Self::new(graph);

        builder.write_header();

        let documents = builder.visible_documents(show_builtins);
        let def_ids = builder.write_document_nodes(&documents);
        let definitions = builder.visible_definitions(&def_ids);
        let visible_def_ids: HashSet<_> = definitions.iter().map(|(_, definition)| definition.id()).collect();
        let decl_ids = builder.write_definition_nodes(&definitions);
        let declarations = builder.visible_declarations(&decl_ids);
        builder.write_declaration_nodes(&declarations);

        builder.write_document_definition_edges(&documents, &visible_def_ids);
        builder.write_definition_declaration_edges(&definitions);
        builder.write_definition_nesting_edges(&definitions, &visible_def_ids);
        builder.write_superclass_edges(&definitions, &decl_ids);
        builder.write_mixin_edges(&definitions, &decl_ids);
        builder.write_member_edges(&declarations, &decl_ids);

        builder.writeln("}");
        builder.output
    }

    fn write_header(&mut self) {
        self.writeln("digraph rubydex {");
        self.writeln("  rankdir=LR");
        self.writeln("  graph [ranksep=0.30 nodesep=0.08 concentrate=true]");
        self.writeln("  node [fontname=\"Courier\" fontsize=10 shape=box]");
        self.writeln("  edge [fontsize=9 fontname=\"Courier\"]");
        self.output.push('\n');
    }

    fn visible_documents(&self, show_builtins: bool) -> Vec<&'a Document> {
        let mut documents: Vec<_> = self
            .graph
            .documents()
            .values()
            .filter(|d| show_builtins || d.uri() != built_in::BUILT_IN_URI)
            .collect();
        documents.sort_by(|a, b| a.uri().cmp(b.uri()));
        documents
    }

    fn write_document_nodes(&mut self, documents: &[&'a Document]) -> HashSet<DefinitionId> {
        let mut def_ids = HashSet::new();
        for document in documents {
            document.to_dot(self);
            for def_id in document.definitions() {
                def_ids.insert(*def_id);
            }
        }
        self.output.push('\n');
        def_ids
    }

    fn visible_definitions(&self, def_ids: &HashSet<DefinitionId>) -> Vec<(String, &'a Definition)> {
        let mut definitions: Vec<_> = self
            .graph
            .definitions()
            .iter()
            .filter(|(id, _)| def_ids.contains(*id))
            .filter_map(|(_, definition)| {
                let decl_id = self.graph.definition_to_declaration_id(definition)?;
                let declaration = self.graph.declarations().get(decl_id)?;
                let sort_key = format!("{}({})", definition.kind(), declaration.name());
                Some((sort_key, definition))
            })
            .collect();
        definitions.sort_by(|a, b| a.0.cmp(&b.0));
        definitions
    }

    fn write_definition_nodes(&mut self, definitions: &[(String, &'a Definition)]) -> HashSet<DeclarationId> {
        let mut decl_ids = HashSet::new();
        for (_, definition) in definitions {
            definition.to_dot(self);
            if let Some(decl_id) = self.graph.definition_to_declaration_id(definition) {
                decl_ids.insert(*decl_id);
            }
        }
        self.output.push('\n');
        decl_ids
    }

    fn visible_declarations(&self, decl_ids: &HashSet<DeclarationId>) -> Vec<(&'a DeclarationId, &'a Declaration)> {
        let mut declarations: Vec<_> = self
            .graph
            .declarations()
            .iter()
            .filter(|(id, _)| decl_ids.contains(*id))
            .collect();
        declarations.sort_by(|(_, a), (_, b)| a.name().cmp(b.name()));
        declarations
    }

    fn write_declaration_nodes(&mut self, declarations: &[(&DeclarationId, &Declaration)]) {
        for (_, declaration) in declarations {
            declaration.to_dot(self);
        }
        self.output.push('\n');
    }

    fn write_document_definition_edges(&mut self, documents: &[&Document], def_ids: &HashSet<DefinitionId>) {
        for document in documents {
            let uri = document.uri();
            let doc_id = Self::doc_node_id(uri);
            for def_id in document.definitions() {
                if def_ids.contains(def_id) {
                    let _ = writeln!(
                        self.output,
                        "  {doc_id} -> \"def_{def_id}\" [label=\"defines\" color=\"{DEF_COLOR}\" fontcolor=\"{DEF_COLOR}\"]"
                    );
                }
            }
        }
        self.output.push('\n');
    }

    fn write_definition_declaration_edges(&mut self, definitions: &[(String, &'a Definition)]) {
        for (_, definition) in definitions {
            let def_id = definition.id();
            if let Some(decl_id) = self.graph.definition_to_declaration_id(definition) {
                let decl_node = Self::decl_node_id(*decl_id);
                let _ = writeln!(
                    self.output,
                    "  \"def_{def_id}\" -> {decl_node} [label=\"declares\" color=\"{DECL_COLOR}\" fontcolor=\"{DECL_COLOR}\"]"
                );
            }
        }
        self.output.push('\n');
    }

    fn write_definition_nesting_edges(
        &mut self,
        definitions: &[(String, &'a Definition)],
        def_ids: &HashSet<DefinitionId>,
    ) {
        for (_, definition) in definitions {
            let parent_id = definition.id();
            let children: &[DefinitionId] = match definition {
                Definition::Class(d) => d.members(),
                Definition::Module(d) => d.members(),
                Definition::SingletonClass(d) => d.members(),
                _ => &[],
            };
            for child_id in children {
                if def_ids.contains(child_id) {
                    let _ = writeln!(
                        self.output,
                        "  \"def_{parent_id}\" -> \"def_{child_id}\" [label=\"contains\" style=dashed arrowhead=onormal color=\"{NESTS_COLOR}\" fontcolor=\"{NESTS_COLOR}\"]"
                    );
                }
            }
        }
        self.output.push('\n');
    }

    fn write_superclass_edges(&mut self, definitions: &[(String, &'a Definition)], decl_ids: &HashSet<DeclarationId>) {
        for (_, definition) in definitions {
            let Definition::Class(class_def) = definition else {
                continue;
            };
            let Some(superclass_ref_id) = class_def.superclass_ref() else {
                continue;
            };
            let Some(decl_id) = self.resolve_ref_to_namespace(*superclass_ref_id) else {
                continue;
            };
            if !decl_ids.contains(&decl_id) {
                continue;
            }
            let Some(child_decl_id) = self.graph.definition_to_declaration_id(definition) else {
                continue;
            };

            let child_node = Self::decl_node_id(*child_decl_id);
            let parent_node = Self::decl_node_id(decl_id);
            let _ = writeln!(
                self.output,
                "  {child_node} -> {parent_node} [label=\"inherits\" color=\"{SUPERCLASS_COLOR}\" fontcolor=\"{SUPERCLASS_COLOR}\"]"
            );
        }
        self.output.push('\n');
    }

    fn write_mixin_edges(&mut self, definitions: &[(String, &'a Definition)], decl_ids: &HashSet<DeclarationId>) {
        for (_, definition) in definitions {
            let mixins: &[Mixin] = match definition {
                Definition::Class(d) => d.mixins(),
                Definition::Module(d) => d.mixins(),
                Definition::SingletonClass(d) => d.mixins(),
                _ => &[],
            };
            if mixins.is_empty() {
                continue;
            }
            let Some(decl_id) = self.graph.definition_to_declaration_id(definition) else {
                continue;
            };
            let src_node = Self::decl_node_id(*decl_id);
            for mixin in mixins {
                self.write_mixin_edge(mixin, &src_node, decl_ids);
            }
        }
        self.output.push('\n');
    }

    fn write_mixin_edge(&mut self, mixin: &Mixin, src_node: &str, decl_ids: &HashSet<DeclarationId>) {
        let mixin_label = match mixin {
            Mixin::Include(_) => "includes",
            Mixin::Prepend(_) => "prepends",
            Mixin::Extend(_) => "extends",
        };
        let Some(target_decl_id) = self.resolve_ref_to_namespace(*mixin.constant_reference_id()) else {
            return;
        };
        if !decl_ids.contains(&target_decl_id) {
            return;
        }
        let target_node = Self::decl_node_id(target_decl_id);
        let _ = writeln!(
            self.output,
            "  {src_node} -> {target_node} [label=\"{mixin_label}\" color=\"{MIXIN_COLOR}\" fontcolor=\"{MIXIN_COLOR}\"]"
        );
    }

    fn write_member_edges(
        &mut self,
        declarations: &[(&DeclarationId, &Declaration)],
        decl_ids: &HashSet<DeclarationId>,
    ) {
        for (declaration_id, declaration) in declarations {
            if let Some(namespace) = declaration.as_namespace() {
                let owner_node = Self::decl_node_id(**declaration_id);
                let mut members: Vec<_> = namespace
                    .members()
                    .values()
                    .filter(|id| decl_ids.contains(*id))
                    .collect();
                members.sort();
                for member_id in members {
                    let member_node = Self::decl_node_id(*member_id);
                    let _ = writeln!(
                        self.output,
                        "  {owner_node} -> {member_node} [label=\"owns\" style=dashed arrowhead=onormal color=\"{MEMBER_COLOR}\" fontcolor=\"{MEMBER_COLOR}\"]"
                    );
                }
            }
        }
    }

    fn resolve_ref(&self, ref_id: crate::model::ids::ConstantReferenceId) -> Option<&'a DeclarationId> {
        let constant_ref = self.graph.constant_references().get(&ref_id)?;
        self.graph.name_id_to_declaration_id(*constant_ref.name_id())
    }

    fn resolve_ref_to_namespace(&self, ref_id: crate::model::ids::ConstantReferenceId) -> Option<DeclarationId> {
        self.resolve_to_namespace(*self.resolve_ref(ref_id)?)
    }

    fn resolve_to_namespace(&self, declaration_id: DeclarationId) -> Option<DeclarationId> {
        let mut queue = VecDeque::from([declaration_id]);
        let mut seen = HashSet::new();

        while let Some(current_id) = queue.pop_front() {
            if !seen.insert(current_id) {
                continue;
            }

            match self.graph.declarations().get(&current_id)? {
                Declaration::Namespace(_) => return Some(current_id),
                Declaration::ConstantAlias(_) => {
                    queue.extend(self.graph.alias_targets(&current_id)?);
                }
                _ => {}
            }
        }

        None
    }

    fn doc_node_id(uri: &str) -> String {
        format!("\"doc_{}\"", UriId::from(uri))
    }

    fn decl_node_id(id: DeclarationId) -> String {
        format!("\"decl_{id}\"")
    }
}

pub trait ToDot {
    fn to_dot(&self, builder: &mut DotBuilder);
}

impl ToDot for Document {
    fn to_dot(&self, builder: &mut DotBuilder) {
        let uri = self.uri();
        let label = uri.rsplit('/').next().unwrap_or(uri);
        let node_id = DotBuilder::doc_node_id(uri);
        let html_label = DotBuilder::label("Document", label, DOC_COLOR);
        let _ = writeln!(
            builder.output,
            "  {node_id} [label={html_label} shape=note color=\"{DOC_COLOR}\" fillcolor=\"{DOC_FILL}\" style=filled]"
        );
    }
}

impl ToDot for Definition {
    fn to_dot(&self, builder: &mut DotBuilder) {
        let def_id = self.id();
        let Some(decl_id) = builder.graph().definition_to_declaration_id(self) else {
            return;
        };
        let Some(declaration) = builder.graph().declarations().get(decl_id) else {
            return;
        };

        let type_label = format!("{}Def", self.kind());
        let html_label = DotBuilder::label(&type_label, declaration.name(), DEF_COLOR);
        let _ = writeln!(
            builder.output,
            "  \"def_{def_id}\" [label={html_label} style=rounded color=\"{DEF_COLOR}\" fillcolor=\"{DEF_FILL}\" style=\"rounded,filled\"]"
        );
    }
}

impl ToDot for Declaration {
    fn to_dot(&self, builder: &mut DotBuilder) {
        let type_label = format!("{}Decl", self.kind());
        let declaration_id = DeclarationId::from(self.name());
        let node_id = DotBuilder::decl_node_id(declaration_id);
        let html_label = DotBuilder::label(&type_label, self.name(), DECL_COLOR);
        let _ = writeln!(
            builder.output,
            "  {node_id} [label={html_label} color=\"{DECL_COLOR}\" fillcolor=\"{DECL_FILL}\" style=filled]"
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::ids::DeclarationId;
    use crate::test_utils::GraphTest;

    #[test]
    fn test_dot_generation() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///test.rb",
            "
                class TestClass
                end

                module TestModule
                end
            ",
        );
        context.resolve();
        let dot_output = DotBuilder::generate(context.graph(), true);

        assert!(dot_output.contains("digraph rubydex"));
        assert!(dot_output.contains("  rankdir=LR"));
        assert!(dot_output.contains("  graph [ranksep=0.30 nodesep=0.08 concentrate=true]"));

        // Document nodes
        assert!(dot_output.contains("Document"));
        assert!(dot_output.contains("test.rb"));

        // Definition nodes
        assert!(dot_output.contains("ClassDef"));
        assert!(dot_output.contains("ModuleDef"));

        // Declaration nodes
        assert!(dot_output.contains("ClassDecl"));
        assert!(dot_output.contains("ModuleDecl"));

        // Edges
        assert!(dot_output.contains("defines"));
        assert!(dot_output.contains("declares"));
        assert!(dot_output.contains("owns"));
    }

    #[test]
    fn test_dot_nesting_edges() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///test.rb",
            "
                module Outer
                  class Inner
                  end
                end
            ",
        );
        context.resolve();
        let dot_output = DotBuilder::generate(context.graph(), false);
        assert!(dot_output.contains("contains"));
    }

    #[test]
    fn test_dot_superclass_edges() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///test.rb",
            "
                class Parent
                end

                class Child < Parent
                end
            ",
        );
        context.resolve();
        let dot_output = DotBuilder::generate(context.graph(), false);
        assert!(dot_output.contains("inherits"));
    }

    #[test]
    fn test_dot_superclass_edge_resolves_alias_target() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///test.rb",
            "
                class Base
                end

                AliasedBase = Base

                class Child < AliasedBase
                end
            ",
        );
        context.resolve();
        let dot_output = DotBuilder::generate(context.graph(), false);

        let child_node = format!("\"decl_{}\"", DeclarationId::from("Child"));
        let base_node = format!("\"decl_{}\"", DeclarationId::from("Base"));
        let alias_node = format!("\"decl_{}\"", DeclarationId::from("AliasedBase"));

        assert!(dot_output.contains(&format!("{child_node} -> {base_node} [label=\"inherits\"")));
        assert!(!dot_output.contains(&format!("{child_node} -> {alias_node} [label=\"inherits\"")));
    }

    #[test]
    fn test_dot_mixin_edges() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///test.rb",
            "
                module Mixin
                end

                class Klass
                  include Mixin
                end
            ",
        );
        context.resolve();
        let dot_output = DotBuilder::generate(context.graph(), false);
        assert!(dot_output.contains("includes"));
    }

    #[test]
    fn test_dot_mixin_edge_resolves_alias_target() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///test.rb",
            "
                module Mixin
                end

                AliasMixin = Mixin

                class Klass
                  include AliasMixin
                end
            ",
        );
        context.resolve();
        let dot_output = DotBuilder::generate(context.graph(), false);

        let klass_node = format!("\"decl_{}\"", DeclarationId::from("Klass"));
        let mixin_node = format!("\"decl_{}\"", DeclarationId::from("Mixin"));
        let alias_node = format!("\"decl_{}\"", DeclarationId::from("AliasMixin"));

        assert!(dot_output.contains(&format!("{klass_node} -> {mixin_node} [label=\"includes\"")));
        assert!(!dot_output.contains(&format!("{klass_node} -> {alias_node} [label=\"includes\"")));
    }

    #[test]
    fn test_dot_declaration_node_ids_do_not_collapse_similar_names() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///test.rb",
            "
                module A
                  class B
                  end
                end

                class A__B
                end
            ",
        );
        context.resolve();
        let dot_output = DotBuilder::generate(context.graph(), false);

        let nested_node = format!("\"decl_{}\"", DeclarationId::from("A::B"));
        let underscored_node = format!("\"decl_{}\"", DeclarationId::from("A__B"));

        assert_ne!(nested_node, underscored_node);
        assert!(dot_output.contains(&format!("{nested_node} [")));
        assert!(dot_output.contains(&format!("{underscored_node} [")));
        assert!(!dot_output.contains("\"decl_A__B\" ["));
    }

    #[test]
    fn test_dot_does_not_emit_document_edges_to_hidden_definition_nodes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///test.rb", "def Missing.foo; end");
        context.resolve();
        let dot_output = DotBuilder::generate(context.graph(), false);

        assert!(!dot_output.contains("[label=\"defines\""));
    }

    #[test]
    fn test_dot_reopened_builtin_not_hidden() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///test.rb",
            "
                class Object
                  def test; end
                end
            ",
        );
        context.resolve();
        let dot_output = DotBuilder::generate(context.graph(), false);

        assert!(dot_output.contains("ClassDecl"));
        assert!(dot_output.contains("Object"));
    }
}
