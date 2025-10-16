//! DOT format generator for Graphviz visualization.

use std::fmt::Write;

use super::renderer::{RenderableGraph, RenderableName, RenderableDefinition, RenderableDocument};

const NAME_NODE_SHAPE: &str = "hexagon";
const DEFINITION_NODE_SHAPE: &str = "ellipse";
const URI_NODE_SHAPE: &str = "box";

#[must_use]
pub fn generate(renderable: &RenderableGraph) -> String {
    let mut output = String::new();
    output.push_str("digraph {\n");
    output.push_str("    rankdir=TB;\n\n");

    write_names(&mut output, &renderable.names);
    write_definitions(&mut output, &renderable.definitions);
    write_documents(&mut output, &renderable.documents);

    output.push_str("}\n");
    output
}

fn write_names(output: &mut String, names: &[RenderableName]) {
    for name in names {
        let escaped_name = escape_dot_string(&name.name);
        let node_id = format!("Name:{}", &name.name);
        let _ = writeln!(
            output,
            "    \"{node_id}\" [label=\"{escaped_name}\",shape={NAME_NODE_SHAPE}];"
        );

        for def_id in &name.definition_ids {
            let _ = writeln!(output, "    \"{node_id}\" -> \"{def_id}\" [dir=both];");
        }
    }

    if !names.is_empty() {
        output.push('\n');
    }
}

fn write_definitions(output: &mut String, definitions: &[RenderableDefinition]) {
    for definition in definitions {
        let escaped_label = escape_dot_string(&definition.label);
        let _ = writeln!(
            output,
            "    \"{}\" [label=\"{escaped_label}\",shape={DEFINITION_NODE_SHAPE}];",
            definition.id
        );
    }

    if !definitions.is_empty() {
        output.push('\n');
    }
}

fn write_documents(output: &mut String, documents: &[RenderableDocument]) {
    for document in documents {
        let escaped_uri = escape_dot_string(&document.uri);
        let escaped_label = escape_dot_string(&document.label);
        let _ = writeln!(
            output,
            "    \"{escaped_uri}\" [label=\"{escaped_label}\",shape={URI_NODE_SHAPE}];"
        );

        for def_id in &document.definition_ids {
            let _ = writeln!(output, "    \"{def_id}\" -> \"{escaped_uri}\";");
        }
    }

    if !documents.is_empty() {
        output.push('\n');
    }
}

fn escape_dot_string(s: &str) -> String {
    if s.contains('"') {
        s.replace('"', r#"\""#)
    } else {
        s.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::GraphTest;
    use crate::visualization::renderer::GraphRenderer;

    #[test]
    fn test_dot_generation() {
        let mut graph_test = GraphTest::new();
        graph_test.index_uri(
            "file:///test.rb",
            "
                class TestClass
                end

                module TestModule
                end
            ",
        );

        let rendered = GraphRenderer::new(&graph_test.graph).render();
        let dot_output = generate(&rendered);

        // Verify structure
        assert!(dot_output.starts_with("digraph {\n"));
        assert!(dot_output.contains("rankdir=TB;"));
        assert!(dot_output.ends_with("}\n"));

        // Verify nodes exist
        assert!(dot_output.contains("Name:TestClass"));
        assert!(dot_output.contains("Name:TestModule"));
        assert!(dot_output.contains("Class(TestClass)"));
        assert!(dot_output.contains("Module(TestModule)"));
        assert!(dot_output.contains("test.rb"));

        // Verify shapes
        assert!(dot_output.contains(&format!("shape={NAME_NODE_SHAPE}")));
        assert!(dot_output.contains(&format!("shape={DEFINITION_NODE_SHAPE}")));
        assert!(dot_output.contains(&format!("shape={URI_NODE_SHAPE}")));
    }

    #[test]
    fn test_escape_dot_string() {
        assert_eq!(escape_dot_string("simple"), "simple");
        assert_eq!(escape_dot_string("with\"quote"), "with\\\"quote");
        assert_eq!(escape_dot_string("\"quoted\""), "\\\"quoted\\\"");
    }

    #[test]
    fn test_empty_graph() {
        let rendered = RenderableGraph {
            names: vec![],
            definitions: vec![],
            documents: vec![],
        };

        let dot_output = generate(&rendered);

        assert_eq!(
            dot_output,
            "digraph {\n    rankdir=TB;\n\n}\n"
        );
    }
}