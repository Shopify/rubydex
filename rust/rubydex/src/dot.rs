use std::collections::HashSet;
use std::fmt::Write;

use crate::model::{
    built_in,
    declaration::Declaration,
    definitions::{Definition, Mixin},
    document::Document,
    graph::Graph,
    ids::{DeclarationId, DefinitionId},
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

    fn label(type_name: &str, name: &str, color: &str) -> String {
        format!(
            concat!(
                "<<table border=\"0\" cellborder=\"0\" cellspacing=\"0\" align=\"center\">",
                "<tr><td align=\"center\"><font point-size=\"8\" color=\"{}\">{}</font></td></tr>",
                "<tr><td align=\"center\"><b>{}</b></td></tr>",
                "</table>>",
            ),
            color, type_name, name,
        )
    }

    #[must_use]
    pub fn generate(graph: &'a Graph, show_builtins: bool) -> String {
        let mut builder = Self::new(graph);

        builder.writeln("digraph rubydex {");
        builder.writeln("  rankdir=TB");
        builder.writeln("  node [fontname=\"Courier\" fontsize=10 shape=box]");
        builder.writeln("  edge [fontsize=9 fontname=\"Courier\"]");
        builder.output.push('\n');

        // 1. Collect documents, write nodes, collect definition IDs
        let mut documents: Vec<_> = graph.documents().values()
            .filter(|d| show_builtins || d.uri() != built_in::BUILT_IN_URI)
            .collect();
        documents.sort_by(|a, b| a.uri().cmp(b.uri()));

        let mut def_ids: HashSet<&DefinitionId> = HashSet::new();
        for document in &documents {
            document.to_dot(&mut builder);
            for def_id in document.definitions() {
                def_ids.insert(def_id);
            }
        }
        builder.output.push('\n');

        // 2. Collect definitions from documents, write nodes, collect declaration IDs
        let mut definitions: Vec<_> = graph
            .definitions()
            .iter()
            .filter(|(id, _)| def_ids.contains(id))
            .filter_map(|(_, definition)| {
                let decl_id = graph.definition_to_declaration_id(definition)?;
                let declaration = graph.declarations().get(decl_id)?;
                let sort_key = format!("{}({})", definition.kind(), declaration.name());
                Some((sort_key, definition))
            })
            .collect();
        definitions.sort_by(|a, b| a.0.cmp(&b.0));

        let mut decl_ids: HashSet<&DeclarationId> = HashSet::new();
        for (_, definition) in &definitions {
            definition.to_dot(&mut builder);
            if let Some(decl_id) = graph.definition_to_declaration_id(definition) {
                decl_ids.insert(decl_id);
            }
        }
        builder.output.push('\n');

        // 3. Collect declarations from definitions, write nodes
        let mut declarations: Vec<_> = graph.declarations().iter()
            .filter(|(id, _)| decl_ids.contains(id))
            .map(|(_, decl)| decl)
            .collect();
        declarations.sort_by(|a, b| a.name().cmp(b.name()));
        for declaration in &declarations {
            declaration.to_dot(&mut builder);
        }
        builder.output.push('\n');

        // Document -> Definition edges (defines)
        for document in &documents {
            let uri = document.uri();
            let doc_id = Self::doc_node_id(uri);
            for def_id in document.definitions() {
                if def_ids.contains(def_id) {
                    let _ = writeln!(
                        builder.output,
                        "  {doc_id} -> \"def_{def_id}\" [label=\"defines\" color=\"{DEF_COLOR}\" fontcolor=\"{DEF_COLOR}\"]"
                    );
                }
            }
        }
        builder.output.push('\n');

        // Definition -> Declaration edges (declares)
        for (_, definition) in &definitions {
            let def_id = definition.id();
            if let Some(decl_id) = graph.definition_to_declaration_id(definition) {
                if let Some(declaration) = graph.declarations().get(decl_id) {
                    let decl_node = Self::decl_node_id(declaration.name());
                    let _ = writeln!(
                        builder.output,
                        "  \"def_{def_id}\" -> {decl_node} [label=\"declares\" color=\"{DECL_COLOR}\" fontcolor=\"{DECL_COLOR}\"]"
                    );
                }
            }
        }
        builder.output.push('\n');

        // Definition -> Definition edges (nesting)
        for (_, definition) in &definitions {
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
                        builder.output,
                        "  \"def_{parent_id}\" -> \"def_{child_id}\" [label=\"contains\" style=dashed arrowhead=onormal color=\"{NESTS_COLOR}\" fontcolor=\"{NESTS_COLOR}\"]"
                    );
                }
            }
        }
        builder.output.push('\n');

        // Declaration -> Declaration edges (superclass)
        for (_, definition) in &definitions {
            if let Definition::Class(class_def) = definition {
                if let Some(superclass_ref_id) = class_def.superclass_ref() {
                    if let Some(decl_id) = builder.resolve_ref(superclass_ref_id) {
                        if !decl_ids.contains(decl_id) {
                            continue;
                        }
                        if let Some(declaration) = graph.declarations().get(decl_id) {
                            if let Some(child_decl_id) = graph.definition_to_declaration_id(definition) {
                                if let Some(child_decl) = graph.declarations().get(child_decl_id) {
                                    let child_node = Self::decl_node_id(child_decl.name());
                                    let parent_node = Self::decl_node_id(declaration.name());
                                    let _ = writeln!(
                                        builder.output,
                                        "  {child_node} -> {parent_node} [label=\"inherits\" color=\"{SUPERCLASS_COLOR}\" fontcolor=\"{SUPERCLASS_COLOR}\"]"
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
        builder.output.push('\n');

        // Declaration -> Declaration edges (mixins: include/prepend/extend)
        for (_, definition) in &definitions {
            let mixins: &[Mixin] = match definition {
                Definition::Class(d) => d.mixins(),
                Definition::Module(d) => d.mixins(),
                Definition::SingletonClass(d) => d.mixins(),
                _ => &[],
            };
            if mixins.is_empty() {
                continue;
            }
            let Some(decl_id) = graph.definition_to_declaration_id(definition) else {
                continue;
            };
            let Some(decl) = graph.declarations().get(decl_id) else {
                continue;
            };
            let src_node = Self::decl_node_id(decl.name());
            for mixin in mixins {
                let mixin_label = match mixin {
                    Mixin::Include(_) => "includes",
                    Mixin::Prepend(_) => "prepends",
                    Mixin::Extend(_) => "extends",
                };
                if let Some(target_decl_id) = builder.resolve_ref(mixin.constant_reference_id()) {
                    if !decl_ids.contains(target_decl_id) {
                        continue;
                    }
                    if let Some(target_decl) = graph.declarations().get(target_decl_id) {
                        let target_node = Self::decl_node_id(target_decl.name());
                        let _ = writeln!(
                            builder.output,
                            "  {src_node} -> {target_node} [label=\"{mixin_label}\" color=\"{MIXIN_COLOR}\" fontcolor=\"{MIXIN_COLOR}\"]"
                        );
                    }
                }
            }
        }
        builder.output.push('\n');

        // Declaration -> Declaration edges (member)
        for declaration in &declarations {
            if let Some(namespace) = declaration.as_namespace() {
                let owner_node = Self::decl_node_id(declaration.name());
                let mut members: Vec<_> = namespace.members().values()
                    .filter(|id| decl_ids.contains(id))
                    .collect();
                members.sort();
                for member_id in members {
                    if let Some(member) = graph.declarations().get(member_id) {
                        let member_node = Self::decl_node_id(member.name());
                        let _ = writeln!(
                            builder.output,
                            "  {owner_node} -> {member_node} [label=\"owns\" style=dashed arrowhead=onormal color=\"{MEMBER_COLOR}\" fontcolor=\"{MEMBER_COLOR}\"]"
                        );
                    }
                }
            }
        }

        builder.writeln("}");
        builder.output
    }

    fn resolve_ref(&self, ref_id: &crate::model::ids::ConstantReferenceId) -> Option<&'a DeclarationId> {
        let constant_ref = self.graph.constant_references().get(ref_id)?;
        self.graph.name_id_to_declaration_id(*constant_ref.name_id())
    }

    fn doc_node_id(uri: &str) -> String {
        format!("\"doc_{}\"", uri.replace(|c: char| !c.is_alphanumeric(), "_"))
    }

    fn decl_node_id(name: &str) -> String {
        format!("\"decl_{}\"", name.replace(|c: char| !c.is_alphanumeric(), "_"))
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
        let node_id = DotBuilder::decl_node_id(self.name());
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
