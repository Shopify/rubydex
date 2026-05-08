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

    write_declaration_nodes(&mut output, graph);
    write_definition_nodes(&mut output, graph);
    write_document_nodes(&mut output, graph);

    output.push_str("}\n");
    output
}

fn write_declaration_nodes(output: &mut String, graph: &Graph) {
    let mut declarations: Vec<_> = graph.declarations().values().collect();
    declarations.sort_by(|a, b| a.name().cmp(b.name()));

    for declaration in declarations {
        let name = declaration.name();
        let escaped_name = escape_dot_string(name);
        let node_id = format!("Name:{name}");
        let _ = writeln!(
            output,
            "    \"{node_id}\" [label=\"{escaped_name}\",shape={NAME_NODE_SHAPE}];"
        );

        for def_id in declaration.definitions() {
            let _ = writeln!(output, "    \"{node_id}\" -> \"def_{def_id}\" [dir=both];");
        }
    }

    output.push('\n');
}

fn write_definition_nodes(output: &mut String, graph: &Graph) {
    let mut definitions: Vec<_> = graph
        .definitions()
        .iter()
        .filter_map(|(def_id, definition)| {
            graph
                .declarations()
                .get(graph.definition_to_declaration_id(definition).unwrap())
                .map(|declaration| {
                    let def_type = definition.kind();
                    let escaped_name = escape_dot_string(declaration.name());
                    let label = format!("{def_type}({escaped_name})");
                    let line = format!("    \"def_{def_id}\" [label=\"{label}\",shape={DEFINITION_NODE_SHAPE}];\n");
                    (label, line)
                })
        })
        .collect();

    definitions.sort_by(|a, b| a.0.cmp(&b.0));

    for (_, line) in definitions {
        output.push_str(&line);
    }
    output.push('\n');
}

fn write_document_nodes(output: &mut String, graph: &Graph) {
    let mut documents: Vec<_> = graph.documents().values().collect();
    documents.sort_by(|a, b| a.uri().cmp(b.uri()));

    for document in documents {
        let uri = document.uri();
        let label = uri.rsplit('/').next().unwrap_or(uri);
        let escaped_uri = escape_dot_string(uri);
        let escaped_label = escape_dot_string(label);
        let _ = writeln!(
            output,
            "    \"{escaped_uri}\" [label=\"{escaped_label}\",shape={URI_NODE_SHAPE}];"
        );

        for def_id in document.definitions() {
            let _ = writeln!(output, "    \"def_{def_id}\" -> \"{escaped_uri}\";");
        }
    }
    output.push('\n');
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{model::ids::DeclarationId, test_utils::GraphTest};

    fn create_test_graph() -> GraphTest {
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
        graph_test.resolve();
        graph_test
    }

    /// Finds the first definition ID for the declaration with the given name.
    fn def_id_for(graph: &Graph, name: &str) -> String {
        let decl = graph.declarations().get(&DeclarationId::from(name)).unwrap();
        decl.definitions().first().unwrap().to_string()
    }

    #[test]
    fn test_dot_generation() {
        let context = create_test_graph();
        let dot_output = generate(context.graph());

        let basic_object_def = def_id_for(context.graph(), "BasicObject");
        let class_def = def_id_for(context.graph(), "Class");
        let kernel_def = def_id_for(context.graph(), "Kernel");
        let module_def = def_id_for(context.graph(), "Module");
        let object_def = def_id_for(context.graph(), "Object");
        let test_class_def = def_id_for(context.graph(), "TestClass");
        let test_module_def = def_id_for(context.graph(), "TestModule");

        let expected = format!(
            r#"digraph {{
    rankdir=TB;

    "Name:BasicObject" [label="BasicObject",shape=hexagon];
    "Name:BasicObject" -> "def_{basic_object_def}" [dir=both];
    "Name:Class" [label="Class",shape=hexagon];
    "Name:Class" -> "def_{class_def}" [dir=both];
    "Name:Kernel" [label="Kernel",shape=hexagon];
    "Name:Kernel" -> "def_{kernel_def}" [dir=both];
    "Name:Module" [label="Module",shape=hexagon];
    "Name:Module" -> "def_{module_def}" [dir=both];
    "Name:Object" [label="Object",shape=hexagon];
    "Name:Object" -> "def_{object_def}" [dir=both];
    "Name:TestClass" [label="TestClass",shape=hexagon];
    "Name:TestClass" -> "def_{test_class_def}" [dir=both];
    "Name:TestModule" [label="TestModule",shape=hexagon];
    "Name:TestModule" -> "def_{test_module_def}" [dir=both];

    "def_{basic_object_def}" [label="Class(BasicObject)",shape=ellipse];
    "def_{class_def}" [label="Class(Class)",shape=ellipse];
    "def_{module_def}" [label="Class(Module)",shape=ellipse];
    "def_{object_def}" [label="Class(Object)",shape=ellipse];
    "def_{test_class_def}" [label="Class(TestClass)",shape=ellipse];
    "def_{kernel_def}" [label="Module(Kernel)",shape=ellipse];
    "def_{test_module_def}" [label="Module(TestModule)",shape=ellipse];

    "file:///test.rb" [label="test.rb",shape=box];
    "def_{test_class_def}" -> "file:///test.rb";
    "def_{test_module_def}" -> "file:///test.rb";
    "rubydex:built-in" [label="rubydex:built-in",shape=box];
    "def_{basic_object_def}" -> "rubydex:built-in";
    "def_{kernel_def}" -> "rubydex:built-in";
    "def_{object_def}" -> "rubydex:built-in";
    "def_{module_def}" -> "rubydex:built-in";
    "def_{class_def}" -> "rubydex:built-in";

}}
"#
        );

        assert_eq!(dot_output, expected);
    }
}
