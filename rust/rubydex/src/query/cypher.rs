//! A small Cypher query engine that runs read-only queries directly against the in-memory
//! [`Graph`](crate::model::graph::Graph).
//!
//! Supported subset:
//! - `MATCH` with node patterns `(v:Label {prop: value})` — labels may be a disjunction
//!   (`(v:Class|Module)` matches a node with **any** of the listed labels) — and relationship
//!   patterns `-[:TYPE]->`, `<-[:TYPE]-`, `-[:TYPE]-`, including variable-length `-[:TYPE*min..max]->`.
//! - `WHERE` with `=`, `<>`, `<`, `<=`, `>`, `>=`, `CONTAINS`, `STARTS WITH`, `ENDS WITH`,
//!   combined with `AND`, `OR`, `NOT`.
//! - `RETURN` with `DISTINCT`, `AS` aliases, and the aggregates `count`, `collect`, `min`, `max`,
//!   `sum`, `avg`.
//! - `ORDER BY`, `SKIP`, `LIMIT`.
//!
//! See [`schema`] for the node labels and relationship types exposed to queries.

// The whole Cypher engine — lexer, parser, AST, executor, values, and formatting — lives in the
// graph-independent `cypher-parser` crate. rubydex only provides the `GraphProvider` mapping for its
// `Graph` (in `schema`) and the static schema description (in `schema_info`).
//
// `Query` is the opaque parsed-query object: callers can `parse` a query string once (failing fast
// on syntax errors), then `run_parsed` it against a graph that was built afterwards.
pub use cypher_parser::{CypherError, CypherValue, OutputFormat, Query, ResultSet, execute, parse};

pub mod schema;
pub mod schema_info;

use crate::model::graph::Graph;

/// Parses and executes a Cypher query against the graph, returning the formatted output.
///
/// # Errors
///
/// Returns a [`CypherError`] if the query cannot be parsed or executed.
pub fn run_query(graph: &Graph, query: &str, output_format: OutputFormat) -> Result<String, CypherError> {
    cypher_parser::run_query(graph, query, output_format)
}

/// Executes an already-parsed [`Query`] against the graph and formats the result. Pair with
/// [`parse`] to validate a query before building the graph.
///
/// # Errors
///
/// Returns a [`CypherError`] if the query cannot be executed.
pub fn run_parsed(graph: &Graph, query: &Query, output_format: OutputFormat) -> Result<String, CypherError> {
    let result = cypher_parser::execute(graph, query)?;
    Ok(cypher_parser::format::format(&result, output_format))
}

/// Returns a description of the queryable schema (node labels, relationship types, and properties)
/// in the requested format. The schema is static and does not require a graph.
#[must_use]
pub fn schema(output_format: OutputFormat) -> String {
    schema_info::describe(output_format)
}

#[cfg(test)]
mod tests;
