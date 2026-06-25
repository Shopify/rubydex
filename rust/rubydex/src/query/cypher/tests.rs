use super::run_query;
use crate::model::graph::Graph;
use crate::test_utils::GraphTest;
use cypher_parser::{CypherValue, OutputFormat, ResultSet, execute, parse};

// Parser-only tests live in the `cypher-parser` crate. These exercise the executor and the
// end-to-end query/format path against a real graph.

fn fixture_graph() -> Graph {
    let mut context = GraphTest::new();
    context.index_uri(
        "file:///zoo.rb",
        "
            module Walkable
            end

            class Animal
              def speak; end
            end

            class Dog < Animal
              include Walkable
            end

            class Cat < Animal
            end
        ",
    );
    context.resolve();
    context.into_graph()
}

fn run(graph: &Graph, query: &str) -> ResultSet {
    let parsed = parse(query).unwrap();
    execute(graph, &parsed).unwrap()
}

fn column_strings(result: &ResultSet, column: usize) -> Vec<String> {
    let mut values: Vec<String> = result.rows.iter().map(|row| row[column].to_display_string()).collect();
    values.sort();
    values
}

#[test]
fn scans_declarations_by_label_and_property() {
    let graph = fixture_graph();
    let result = run(&graph, "MATCH (c:Class {name: 'Dog'}) RETURN c.name");
    assert_eq!(result.columns, vec!["c.name".to_string()]);
    assert_eq!(column_strings(&result, 0), vec!["Dog".to_string()]);
}

#[test]
fn scans_label_disjunction() {
    let graph = fixture_graph();
    let result = run(
        &graph,
        "MATCH (n:Class|Module) WHERE n.name = 'Animal' OR n.name = 'Walkable' RETURN n.name, n.kind",
    );
    let names = column_strings(&result, 0);
    assert_eq!(names, vec!["Animal".to_string(), "Walkable".to_string()]);
}

#[test]
fn follows_inherits_relationship() {
    let graph = fixture_graph();
    let result = run(
        &graph,
        "MATCH (c:Class)-[:INHERITS]->(p:Class) WHERE c.name = 'Dog' RETURN p.name",
    );
    assert_eq!(column_strings(&result, 0), vec!["Animal".to_string()]);
}

#[test]
fn follows_incoming_relationship() {
    let graph = fixture_graph();
    let result = run(
        &graph,
        "MATCH (p:Class)<-[:INHERITS]-(c:Class) WHERE p.name = 'Animal' RETURN c.name",
    );
    assert_eq!(column_strings(&result, 0), vec!["Cat".to_string(), "Dog".to_string()]);
}

#[test]
fn follows_includes_relationship() {
    let graph = fixture_graph();
    let result = run(
        &graph,
        "MATCH (c:Class)-[:INCLUDES]->(m) WHERE c.name = 'Dog' RETURN m.name",
    );
    assert_eq!(column_strings(&result, 0), vec!["Walkable".to_string()]);
}

#[test]
fn follows_owns_to_method() {
    let graph = fixture_graph();
    let result = run(
        &graph,
        "MATCH (c:Class)-[:OWNS]->(m:Method) WHERE c.name = 'Animal' RETURN m.unqualified_name",
    );
    assert!(column_strings(&result, 0).iter().any(|name| name.contains("speak")));
}

#[test]
fn variable_length_ancestor_chain() {
    let graph = fixture_graph();
    let result = run(
        &graph,
        "MATCH (c:Class)-[:ANCESTOR]->(a) WHERE c.name = 'Dog' RETURN a.name",
    );
    let ancestors = column_strings(&result, 0);
    assert!(ancestors.contains(&"Animal".to_string()));
    assert!(ancestors.contains(&"Walkable".to_string()));
    assert!(ancestors.contains(&"Object".to_string()));
}

#[test]
fn traverses_document_to_declaration() {
    let graph = fixture_graph();
    let result = run(
        &graph,
        "MATCH (d:Document)-[:DEFINES]->(def:Definition)-[:DECLARES]->(decl) WHERE decl.name = 'Dog' RETURN decl.name",
    );
    assert_eq!(column_strings(&result, 0), vec!["Dog".to_string()]);
}

#[test]
fn aggregation_counts_subclasses() {
    let graph = fixture_graph();
    let result = run(
        &graph,
        "MATCH (c:Class)-[:INHERITS]->(p:Class) WHERE p.name = 'Animal' RETURN p.name, count(c) AS subclasses",
    );
    assert_eq!(result.rows.len(), 1);
    assert_eq!(result.rows[0][0], CypherValue::Str("Animal".into()));
    assert_eq!(result.rows[0][1], CypherValue::Int(2));
}

#[test]
fn distinct_and_order_and_limit() {
    let graph = fixture_graph();
    let result = run(
        &graph,
        "MATCH (c:Class)-[:INHERITS]->(p:Class) RETURN DISTINCT p.name ORDER BY p.name LIMIT 1",
    );
    assert_eq!(result.rows.len(), 1);
    assert_eq!(result.rows[0][0], CypherValue::Str("Animal".into()));
}

#[test]
fn where_with_boolean_operators() {
    let graph = fixture_graph();
    let result = run(
        &graph,
        "MATCH (c:Class) WHERE c.name = 'Dog' OR c.name = 'Cat' RETURN c.name",
    );
    assert_eq!(column_strings(&result, 0), vec!["Cat".to_string(), "Dog".to_string()]);
}

#[test]
fn run_query_table_output() {
    let graph = fixture_graph();
    let output = run_query(
        &graph,
        "MATCH (c:Class {name: 'Dog'}) RETURN c.name",
        OutputFormat::Table,
    )
    .unwrap();
    assert!(output.contains("c.name"));
    assert!(output.contains("Dog"));
    assert!(output.contains("1 row"));
}

#[test]
fn run_query_json_output() {
    let graph = fixture_graph();
    let output = run_query(
        &graph,
        "MATCH (c:Class {name: 'Dog'}) RETURN c.name",
        OutputFormat::Json,
    )
    .unwrap();
    assert_eq!(output, "[{\"c.name\":\"Dog\"}]");
}

#[test]
fn unknown_relationship_type_errors() {
    let graph = fixture_graph();
    let parsed = parse("MATCH (a)-[:BOGUS]->(b) RETURN a").unwrap();
    assert!(execute(&graph, &parsed).is_err());
}

#[test]
fn document_uri_path_and_name_are_distinct() {
    let graph = fixture_graph();
    let result = run(
        &graph,
        "MATCH (d:Document) WHERE d.uri = 'file:///zoo.rb' RETURN d.uri, d.path, d.name",
    );
    assert_eq!(
        result.columns,
        vec!["d.uri".to_string(), "d.path".to_string(), "d.name".to_string()]
    );
    // `uri` is the full URI and `name` is the basename on every platform.
    assert_eq!(column_strings(&result, 0), vec!["file:///zoo.rb".to_string()]);
    assert_eq!(column_strings(&result, 2), vec!["zoo.rb".to_string()]);

    // `path` is the decoded file-system path. A drive-less `file://` URI has no valid Windows path,
    // so there it falls back to the raw URI; on Unix it decodes to `/zoo.rb`.
    #[cfg(not(windows))]
    assert_eq!(column_strings(&result, 1), vec!["/zoo.rb".to_string()]);
    #[cfg(windows)]
    assert_eq!(column_strings(&result, 1), vec!["file:///zoo.rb".to_string()]);
}
