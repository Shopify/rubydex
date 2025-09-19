//! Integrity checking for the Graph
//!
//! This module provides mechanisms to validate the internal consistency of the Graph structure
//! by applying a set of integrity rules.
//!
//! This shouldn't be used for production code, but is useful for testing and debugging.

use super::graph::Graph;
use std::fmt;

type ValidatorFn = Box<dyn Fn(&Graph, &mut Vec<String>) + Send + Sync>;

/// An integrity rule that can be applied to a Graph to validate its consistency
struct IntegrityRule {
    /// Human-readable description of what this rule checks
    pub description: String,
    /// The validation function that takes a Graph and appends errors to the error list
    pub validator: ValidatorFn,
}

impl IntegrityRule {
    pub fn new<F>(description: &str, validator: F) -> Self
    where
        F: Fn(&Graph, &mut Vec<String>) + Send + Sync + 'static,
    {
        Self {
            description: description.into(),
            validator: Box::new(validator),
        }
    }

    /// Applies this rule to the given index, appending any errors to the provided vector
    pub fn apply(&self, graph: &Graph, errors: &mut Vec<String>) {
        (self.validator)(graph, errors);
    }
}

impl fmt::Debug for IntegrityRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IntegrityRule")
            .field("description", &self.description)
            .field("validator", &"<function>")
            .finish()
    }
}

#[derive(Debug, Default)]
pub struct IntegrityChecker {
    rules: Vec<IntegrityRule>,
}

impl IntegrityChecker {
    #[must_use]
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    pub fn add_rule<F>(&mut self, description: &str, validator: F)
    where
        F: Fn(&Graph, &mut Vec<String>) + Send + Sync + 'static,
    {
        self.rules.push(IntegrityRule::new(description, validator));
    }

    /// Applies all rules to the given index and returns all errors found
    #[must_use]
    pub fn apply(&self, graph: &Graph) -> Vec<String> {
        let mut errors = Vec::new();
        for rule in &self.rules {
            rule.apply(graph, &mut errors);
        }
        errors
    }

    /// Applies all rules and panics if any fail (for debugging/testing)
    ///
    /// # Panics
    ///
    /// Panics if any integrity rule fails
    #[cfg(test)]
    pub fn assert_integrity(&self, graph: &Graph) {
        let errors = self.apply(graph);
        assert!(
            errors.is_empty(),
            "Integrity check failed with {} errors:\n{}",
            errors.len(),
            errors.join("\n")
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::definitions::{Definition, ModuleDefinition};
    use crate::model::graph::Graph;

    use crate::model::ids::NameId;
    use crate::offset::Offset;

    #[test]
    fn test_integrity_check_on_empty_index() {
        let checker = IntegrityChecker::new();
        let graph = Graph::new();

        checker.assert_integrity(&graph);
    }

    #[test]
    fn test_integrity_check_with_custom_rule() {
        let mut checker = IntegrityChecker::new();

        checker.add_rule("Index must be empty", |index, errors| {
            if !index.declarations().is_empty() {
                errors.push("Index is not empty".to_string());
            }
        });

        let mut graph = Graph::new();

        // Should pass since the index is empty
        checker.assert_integrity(&graph);

        let uri_id = graph.add_document("file:///foo.rb".to_string(), None);
        let name_id = NameId::from("Foo");
        let definition = Definition::Module(Box::new(ModuleDefinition::new(
            name_id,
            uri_id,
            Offset::new(0, 15),
            String::new(),
        )));
        graph.add_definition("Foo".to_string(), definition);

        // Should fail since the index is not empty
        let errors = checker.apply(&graph);
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0], "Index is not empty");
    }
}
