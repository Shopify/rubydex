use super::ast::{AggFn, CmpOp, Direction, Expr, Literal};
use super::executor::{self, ResultSet};
use super::parser;
use super::value::CypherValue;
use super::{OutputFormat, run_query};
use crate::model::graph::Graph;
use crate::test_utils::GraphTest;

// ---- Parser tests --------------------------------------------------------

#[test]
fn parses_basic_match_return() {
    let query = parser::parse("MATCH (c:Class) RETURN c.name").unwrap();
    assert_eq!(query.patterns.len(), 1);
    let start = &query.patterns[0].start;
    assert_eq!(start.var.as_deref(), Some("c"));
    assert_eq!(start.labels, vec!["Class".to_string()]);
    assert!(query.patterns[0].rest.is_empty());
    assert_eq!(query.return_clause.items.len(), 1);
    assert_eq!(
        query.return_clause.items[0].expr,
        Expr::Property("c".into(), "name".into())
    );
}

#[test]
fn parses_label_disjunction() {
    let query = parser::parse("MATCH (n:Class|Module) RETURN n").unwrap();
    assert_eq!(
        query.patterns[0].start.labels,
        vec!["Class".to_string(), "Module".to_string()]
    );
}

#[test]
fn parses_inline_properties() {
    let query = parser::parse("MATCH (c:Class {name: 'Foo'}) RETURN c").unwrap();
    let props = &query.patterns[0].start.props;
    assert_eq!(props.len(), 1);
    assert_eq!(props[0].0, "name");
    assert_eq!(props[0].1, Literal::Str("Foo".into()));
}

#[test]
fn parses_relationship_directions() {
    let outgoing = parser::parse("MATCH (a)-[:INHERITS]->(b) RETURN a").unwrap();
    assert_eq!(outgoing.patterns[0].rest[0].0.direction, Direction::Outgoing);
    assert_eq!(outgoing.patterns[0].rest[0].0.types, vec!["INHERITS".to_string()]);

    let incoming = parser::parse("MATCH (a)<-[:INHERITS]-(b) RETURN a").unwrap();
    assert_eq!(incoming.patterns[0].rest[0].0.direction, Direction::Incoming);

    let both = parser::parse("MATCH (a)-[:INHERITS]-(b) RETURN a").unwrap();
    assert_eq!(both.patterns[0].rest[0].0.direction, Direction::Both);
}

#[test]
fn parses_variable_length() {
    let query = parser::parse("MATCH (a)-[:INHERITS*2..5]->(b) RETURN a").unwrap();
    let length = query.patterns[0].rest[0].0.length.unwrap();
    assert_eq!(length.min, 2);
    assert_eq!(length.max, Some(5));

    let unbounded = parser::parse("MATCH (a)-[:OWNS*]->(b) RETURN a").unwrap();
    let length = unbounded.patterns[0].rest[0].0.length.unwrap();
    assert_eq!(length.min, 1);
    assert_eq!(length.max, None);

    let exact = parser::parse("MATCH (a)-[:OWNS*3]->(b) RETURN a").unwrap();
    let length = exact.patterns[0].rest[0].0.length.unwrap();
    assert_eq!(length.min, 3);
    assert_eq!(length.max, Some(3));
}

#[test]
fn parses_aggregation_and_alias() {
    let query = parser::parse("MATCH (c:Class) RETURN c.name, count(*) AS total").unwrap();
    assert_eq!(query.return_clause.items[1].alias.as_deref(), Some("total"));
    assert_eq!(
        query.return_clause.items[1].expr,
        Expr::Aggregate {
            func: AggFn::Count,
            arg: None,
            distinct: false,
        }
    );
}

#[test]
fn parses_where_and_order_limit() {
    let query =
        parser::parse("MATCH (c:Class) WHERE c.name CONTAINS 'Service' RETURN c.name ORDER BY c.name DESC LIMIT 5")
            .unwrap();
    let Some(Expr::Compare(_, op, _)) = query.where_clause else {
        panic!("expected comparison");
    };
    assert_eq!(op, CmpOp::Contains);
    assert_eq!(query.order_by.len(), 1);
    assert!(query.order_by[0].descending);
    assert_eq!(query.limit, Some(5));
}

#[test]
fn rejects_invalid_syntax() {
    assert!(parser::parse("MATCH (c:Class RETURN c").is_err());
    assert!(parser::parse("RETURN c").is_err());
    assert!(parser::parse("MATCH (c) RETURN").is_err());
    assert!(parser::parse("MATCH (a)<-[:INHERITS]->(b) RETURN a").is_err());
}

// ---- Executor tests ------------------------------------------------------

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
    let parsed = parser::parse(query).unwrap();
    executor::execute(graph, &parsed).unwrap()
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
    let parsed = parser::parse("MATCH (a)-[:BOGUS]->(b) RETURN a").unwrap();
    assert!(executor::execute(&graph, &parsed).is_err());
}
