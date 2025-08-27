//! DOT format generator for Graphviz visualization of the graph structure.

use std::fmt::Write;

use crate::model::graph::Graph;

const NAME_NODE_SHAPE: &str = "hexagon";
const DEFINITION_NODE_SHAPE: &str = "ellipse";
const URI_NODE_SHAPE: &str = "box";

/// Escapes a string for use in DOT format labels and identifiers.
fn escape_dot_string(s: &str) -> String {
    if !s.contains('"') {
        return s.to_string();
    }

    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            _ => result.push(c),
        }
    }
    result
}

#[must_use]
pub fn generate(graph: &Graph) -> String {
    let mut output = String::new();
    output.push_str("digraph {\n");
    output.push_str("    rankdir=TB;\n\n");

    write_name_nodes(&mut output, graph);
    write_definition_nodes(&mut output, graph);
    write_uri_nodes(&mut output, graph);
    write_name_to_definition_edges(&mut output, graph);
    write_definition_to_uri_edges(&mut output, graph);

    output.push_str("}\n");
    output
}

fn write_name_nodes(output: &mut String, graph: &Graph) {
    let mut names: Vec<_> = graph.names().values().collect();
    names.sort();

    for name in names {
        let escaped_name = escape_dot_string(name);
        let node_id = format!("Name:{name}");
        let _ = writeln!(
            output,
            "    \"{node_id}\" [label=\"{escaped_name}\",shape={NAME_NODE_SHAPE}];"
        );
    }
    output.push('\n');
}

fn write_definition_nodes(output: &mut String, graph: &Graph) {
    let mut definitions: Vec<_> = graph.definitions().iter().collect();
    definitions.sort_by_key(|(def_id, _)| def_id.to_string());

    for (def_id, definition) in definitions {
        if let Some(name_id) = graph.definition_to_name().get(def_id)
            && let Some(name) = graph.names().get(name_id)
        {
            let def_type = definition.kind();
            let escaped_name = escape_dot_string(name);

            let _ = writeln!(
                output,
                "    \"def_{def_id}\" [label=\"{def_type}({escaped_name})\",shape={DEFINITION_NODE_SHAPE}];"
            );
        }
    }
    output.push('\n');
}

fn write_uri_nodes(output: &mut String, graph: &Graph) {
    let mut uris: Vec<_> = graph.uri_pool().values().collect();
    uris.sort();

    for uri in uris {
        let label = uri.rsplit('/').next().unwrap_or(uri);
        let escaped_uri = escape_dot_string(uri);
        let escaped_label = escape_dot_string(label);
        let _ = writeln!(
            output,
            "    \"{escaped_uri}\" [label=\"{escaped_label}\",shape={URI_NODE_SHAPE}];"
        );
    }
    output.push('\n');
}

fn write_name_to_definition_edges(output: &mut String, graph: &Graph) {
    let mut name_to_def_edges: Vec<_> = Vec::new();

    for (name_id, definition_ids) in graph.name_to_definitions() {
        if let Some(name) = graph.names().get(name_id) {
            for def_id in definition_ids {
                if graph.definitions().contains_key(def_id) {
                    name_to_def_edges.push((name.clone(), def_id.to_string()));
                }
            }
        }
    }
    name_to_def_edges.sort();

    for (name, def_id) in name_to_def_edges {
        let name_node = format!("Name:{}", escape_dot_string(&name));
        let _ = writeln!(output, "    \"{name_node}\" -> \"def_{def_id}\" [dir=both];");
    }
}

fn write_definition_to_uri_edges(output: &mut String, graph: &Graph) {
    let mut uri_edges: Vec<_> = graph.uris_to_definitions().iter().collect();
    uri_edges.sort_by_key(|(uri_id, _)| uri_id.to_string());

    for (uri_id, definition_ids) in uri_edges {
        if let Some(uri) = graph.uri_pool().get(uri_id) {
            let escaped_uri = escape_dot_string(uri);
            let mut sorted_def_ids: Vec<_> = definition_ids.iter().collect();
            sorted_def_ids.sort_by_key(std::string::ToString::to_string);

            for def_id in sorted_def_ids {
                if graph.definitions().contains_key(def_id) {
                    let _ = writeln!(output, "    \"def_{def_id}\" -> \"{escaped_uri}\";");
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::GraphTest;

    fn create_test_graph() -> Graph {
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
        graph_test.graph
    }

    #[test]
    fn test_dot_generation() {
        let graph = create_test_graph();
        let dot_output = generate(&graph);

        let class_def_id = graph
            .definitions()
            .iter()
            .find(|(_, def)| matches!(def, crate::model::definitions::Definition::Class(_)))
            .map(|(id, _)| id.to_string())
            .unwrap();

        let module_def_id = graph
            .definitions()
            .iter()
            .find(|(_, def)| matches!(def, crate::model::definitions::Definition::Module(_)))
            .map(|(id, _)| id.to_string())
            .unwrap();

        let expected = format!(
            r#"digraph {{
    rankdir=TB;

    "Name:TestClass" [label="TestClass",shape=hexagon];
    "Name:TestModule" [label="TestModule",shape=hexagon];

    "def_{module_def_id}" [label="Module(TestModule)",shape=ellipse];
    "def_{class_def_id}" [label="Class(TestClass)",shape=ellipse];

    "file:///test.rb" [label="test.rb",shape=box];

    "Name:TestClass" -> "def_{class_def_id}" [dir=both];
    "Name:TestModule" -> "def_{module_def_id}" [dir=both];
    "def_{module_def_id}" -> "file:///test.rb";
    "def_{class_def_id}" -> "file:///test.rb";
}}
"#
        );

        assert_eq!(dot_output, expected);
    }
}
