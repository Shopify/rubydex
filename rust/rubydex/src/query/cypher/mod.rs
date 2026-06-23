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

// The lexer, parser, AST, and error types live in the graph-independent `cypher-parser` crate.
// They are re-exported here so the rest of the engine (and `super::` paths) keep working unchanged.
pub use cypher_parser::{CypherError, ast, error, parser};

pub mod executor;
pub mod format;
pub mod schema;
pub mod schema_info;
pub mod value;

pub use executor::ResultSet;
pub use format::OutputFormat;

use crate::model::graph::Graph;

/// Parses and executes a Cypher query against the graph, returning the formatted output.
///
/// # Errors
///
/// Returns a [`CypherError`] if the query cannot be parsed or executed.
pub fn run_query(graph: &Graph, query: &str, output_format: OutputFormat) -> Result<String, CypherError> {
    let parsed = parser::parse(query)?;
    let result = executor::execute(graph, &parsed)?;
    Ok(format::format(&result, output_format))
}

/// Returns a description of the queryable schema (node labels, relationship types, and properties)
/// in the requested format. The schema is static and does not require a graph.
#[must_use]
pub fn schema(output_format: OutputFormat) -> String {
    schema_info::describe(output_format)
}

#[cfg(test)]
mod tests;
