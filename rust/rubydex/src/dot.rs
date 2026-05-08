use std::fmt::Write;

use crate::model::{
    declaration::Declaration,
    definitions::Definition,
    document::Document,
    graph::Graph,
};

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

    fn label(type_name: &str, name: &str) -> String {
        format!(
            concat!(
                "<<table border=\"0\" cellborder=\"0\" cellspacing=\"0\" align=\"center\">",
                "<tr><td align=\"center\"><font point-size=\"8\">{}</font></td></tr>",
                "<tr><td align=\"center\"><b>{}</b></td></tr>",
                "</table>>",
            ),
            type_name, name,
        )
    }

    #[must_use]
    pub fn generate(graph: &'a Graph) -> String {
        let mut builder = Self::new(graph);

        builder.writeln("digraph rubydex {");
        builder.writeln("  rankdir=TB");
        builder.writeln("  node [fontname=\"Courier\" fontsize=10 shape=box]");
        builder.writeln("  edge [fontsize=9 fontname=\"Courier\"]");
        builder.output.push('\n');

        let mut documents: Vec<_> = graph.documents().values().collect();
        documents.sort_by(|a, b| a.uri().cmp(b.uri()));
        for document in &documents {
            document.to_dot(&mut builder);
        }
        builder.output.push('\n');

        let mut definitions: Vec<_> = graph
            .definitions()
            .iter()
            .filter_map(|(_, definition)| {
                let decl_id = graph.definition_to_declaration_id(definition)?;
                let declaration = graph.declarations().get(decl_id)?;
                let sort_key = format!("{}({})", definition.kind(), declaration.name());
                Some((sort_key, definition))
            })
            .collect();
        definitions.sort_by(|a, b| a.0.cmp(&b.0));
        for (_, definition) in &definitions {
            definition.to_dot(&mut builder);
        }
        builder.output.push('\n');

        let mut declarations: Vec<_> = graph.declarations().values().collect();
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
                let _ = writeln!(
                    builder.output,
                    "  {doc_id} -> \"def_{def_id}\" [label=\"defines\"]"
                );
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
                        "  \"def_{def_id}\" -> {decl_node} [label=\"declares\"]"
                    );
                }
            }
        }
        builder.output.push('\n');

        // Declaration -> Declaration edges (member)
        for declaration in &declarations {
            if let Some(namespace) = declaration.as_namespace() {
                let owner_node = Self::decl_node_id(declaration.name());
                let mut members: Vec<_> = namespace.members().values().collect();
                members.sort();
                for member_id in members {
                    if let Some(member) = graph.declarations().get(member_id) {
                        let member_node = Self::decl_node_id(member.name());
                        let _ = writeln!(
                            builder.output,
                            "  {owner_node} -> {member_node} [label=\"member\"]"
                        );
                    }
                }
            }
        }

        builder.writeln("}");
        builder.output
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
        let html_label = DotBuilder::label("Document", label);
        let _ = writeln!(
            builder.output,
            "  {node_id} [label={html_label} shape=note]"
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
        let html_label = DotBuilder::label(&type_label, declaration.name());
        let _ = writeln!(
            builder.output,
            "  \"def_{def_id}\" [label={html_label} style=rounded]"
        );
    }
}

impl ToDot for Declaration {
    fn to_dot(&self, builder: &mut DotBuilder) {
        let type_label = format!("{}Decl", self.kind());
        let node_id = DotBuilder::decl_node_id(self.name());
        let html_label = DotBuilder::label(&type_label, self.name());
        let _ = writeln!(
            builder.output,
            "  {node_id} [label={html_label}]"
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
        let dot_output = DotBuilder::generate(context.graph());

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
        assert!(dot_output.contains("member"));
    }
}
