//! Graph rendering preparation and filtering.

use std::collections::HashSet;
use std::fmt;

use crate::model::graph::Graph;
use crate::model::ids::{DeclarationId, DefinitionId, UriId};

#[derive(Debug)]
pub enum FilterError {
    NameNotFound { name: String },
}

impl fmt::Display for FilterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self::NameNotFound { name } = self;
        write!(f, "No declarations found for name '{name}'")
    }
}

impl std::error::Error for FilterError {}

/// A prepared representation of graph elements ready for rendering.
#[derive(Debug)]
pub struct RenderableGraph {
    pub names: Vec<RenderableName>,
    pub definitions: Vec<RenderableDefinition>,
    pub documents: Vec<RenderableDocument>,
}

#[derive(Debug)]
pub struct RenderableName {
    pub name: String,
    pub definition_ids: Vec<String>,
}

#[derive(Debug)]
pub struct RenderableDefinition {
    pub id: String,    // "def_123"
    pub label: String, // "Class(RubyLsp::Server)"
}

#[derive(Debug)]
pub struct RenderableDocument {
    pub uri: String,
    pub label: String, // Filename extracted from URI
    pub definition_ids: Vec<String>,
}

pub struct GraphRenderer<'graph> {
    graph: &'graph Graph,
    filter: Option<FilteredView>,
}

impl<'graph> GraphRenderer<'graph> {
    #[must_use]
    pub fn new(graph: &'graph Graph) -> Self {
        Self { graph, filter: None }
    }

    /// # Errors
    ///
    /// Returns `FilterError::NameNotFound` if the name doesn't exist in the graph.
    pub fn with_name_filter(mut self, name: &str) -> Result<Self, FilterError> {
        let declaration_id = DeclarationId::from(name);

        let declaration = self
            .graph
            .declarations()
            .get(&declaration_id)
            .ok_or_else(|| FilterError::NameNotFound { name: name.to_string() })?;

        let mut filtered = FilteredView::new();
        filtered.declaration_ids.insert(declaration_id);

        for def_id in declaration.definitions() {
            filtered.definition_ids.insert(*def_id);

            if let Some(definition) = self.graph.definitions().get(def_id) {
                filtered.uri_ids.insert(*definition.uri_id());
            }
        }

        self.filter = Some(filtered);
        Ok(self)
    }

    #[must_use]
    pub fn render(self) -> RenderableGraph {
        let mut names = Vec::new();
        let mut definitions = Vec::new();
        let mut documents = Vec::new();

        let is_included = |id: &DefinitionId| self.filter.as_ref().is_none_or(|f| f.definition_ids.contains(id));

        for (declaration_id, declaration) in self.graph.declarations() {
            if !self.filter.as_ref().is_none_or(|f| f.declaration_ids.contains(declaration_id)) {
                continue;
            }

            let def_ids: Vec<String> = declaration
                .definitions()
                .iter()
                .filter(|def_id| is_included(def_id))
                .map(|def_id| format_def_id(*def_id))
                .collect();

            names.push(RenderableName {
                name: declaration.name().to_string(),
                definition_ids: def_ids,
            });
        }

        for (def_id, definition) in self.graph.definitions() {
            if !is_included(def_id) {
                continue;
            }

            if let Some(declaration) = self.graph.declarations().get(definition.declaration_id()) {
                definitions.push(RenderableDefinition {
                    id: format_def_id(*def_id),
                    label: format!("{}({})", definition.kind(), declaration.name()),
                });
            }
        }

        for (uri_id, document) in self.graph.documents() {
            if !self.filter.as_ref().is_none_or(|f| f.uri_ids.contains(uri_id)) {
                continue;
            }

            let def_ids: Vec<String> = document
                .definitions()
                .iter()
                .filter(|def_id| is_included(def_id))
                .map(|def_id| format_def_id(*def_id))
                .collect();

            let label = extract_filename(document.uri());

            documents.push(RenderableDocument {
                uri: document.uri().to_string(),
                label: label.to_string(),
                definition_ids: def_ids,
            });
        }

        names.sort_by(|a, b| a.name.cmp(&b.name));
        definitions.sort_by(|a, b| a.label.cmp(&b.label));
        documents.sort_by(|a, b| a.uri.cmp(&b.uri));

        RenderableGraph {
            names,
            definitions,
            documents,
        }
    }
}

#[allow(clippy::struct_field_names)]
struct FilteredView {
    declaration_ids: HashSet<DeclarationId>,
    definition_ids: HashSet<DefinitionId>,
    uri_ids: HashSet<UriId>,
}

impl FilteredView {
    fn new() -> Self {
        Self {
            declaration_ids: HashSet::new(),
            definition_ids: HashSet::new(),
            uri_ids: HashSet::new(),
        }
    }
}

fn format_def_id(id: DefinitionId) -> String {
    format!("def_{id}")
}

fn extract_filename(uri: &str) -> &str {
    uri.rsplit('/').next().unwrap_or(uri)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::GraphTest;

    #[test]
    fn render_full_graph() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "class Foo; end");
        context.index_uri("file:///bar.rb", "class Bar; end");

        let rendered = GraphRenderer::new(&context.graph).render();

        // The graph now includes <main> as well as Bar and Foo
        assert_eq!(rendered.names.len(), 3);
        assert_eq!(rendered.definitions.len(), 2);
        assert_eq!(rendered.documents.len(), 2);

        // Check sorting - names are sorted alphabetically
        assert_eq!(rendered.names[0].name, "<main>");
        assert_eq!(rendered.names[1].name, "Bar");
        assert_eq!(rendered.names[2].name, "Foo");
    }

    #[test]
    fn render_filtered_graph() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "class Foo; end");
        context.index_uri("file:///bar.rb", "class Bar; end");

        let rendered = GraphRenderer::new(&context.graph)
            .with_name_filter("Foo")
            .unwrap()
            .render();

        assert_eq!(rendered.names.len(), 1);
        assert_eq!(rendered.names[0].name, "Foo");
        assert_eq!(rendered.definitions.len(), 1);
        assert_eq!(rendered.documents.len(), 1);
        assert!(rendered.documents[0].uri.contains("foo.rb"));
    }

    #[test]
    fn filter_nonexistent_name() {
        let context = GraphTest::new();
        let result = GraphRenderer::new(&context.graph).with_name_filter("NonExistent");

        assert!(matches!(result, Err(FilterError::NameNotFound { .. })));
    }

    #[test]
    fn render_includes_all_definitions() {
        let mut context = GraphTest::new();
        // Class reopening - same name in two files
        context.index_uri("file:///foo1.rb", "class Foo; end");
        context.index_uri("file:///foo2.rb", "class Foo; end");

        let rendered = GraphRenderer::new(&context.graph)
            .with_name_filter("Foo")
            .unwrap()
            .render();

        assert_eq!(rendered.names.len(), 1);
        assert_eq!(rendered.definitions.len(), 2);
        assert_eq!(rendered.documents.len(), 2);
    }
}

