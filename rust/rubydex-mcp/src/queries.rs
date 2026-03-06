use std::collections::HashMap;
use std::path::{Path, PathBuf};

use rubydex::model::declaration::{Ancestor, Ancestors, Declaration, Namespace};
use rubydex::model::graph::Graph;
use rubydex::model::ids::{DeclarationId, UriId};
use serde::Serialize;
use url::Url;

// ---------------------------------------------------------------------------
// Error types
// ---------------------------------------------------------------------------

#[derive(Debug, PartialEq)]
pub enum QueryError {
    NotFound {
        name: String,
    },
    InvalidKind {
        name: String,
        actual_kind: String,
        tool_name: &'static str,
    },
    InvalidPath {
        path: String,
    },
}

impl QueryError {
    pub fn to_json_string(&self) -> String {
        let (error, message, suggestion) = match self {
            QueryError::NotFound { name } => (
                "not_found",
                format!("Declaration '{name}' not found"),
                "Try search_declarations with a partial name to find the correct FQN".to_string(),
            ),
            QueryError::InvalidKind {
                name,
                actual_kind,
                tool_name,
            } => (
                "invalid_kind",
                format!("'{name}' is not a class or module (it is a {actual_kind})"),
                format!("{tool_name} only works on classes and modules, not methods or constants"),
            ),
            QueryError::InvalidPath { path } => (
                "invalid_path",
                format!("Cannot convert '{path}' to a file URI"),
                "Use a relative path like 'app/models/user.rb' or an absolute path".to_string(),
            ),
        };
        serde_json::to_string(&serde_json::json!({
            "error": error,
            "message": message,
            "suggestion": suggestion,
        }))
        .unwrap_or_else(|_| "{}".to_string())
    }
}

// ---------------------------------------------------------------------------
// Result types
// ---------------------------------------------------------------------------

#[derive(Debug, Serialize, PartialEq)]
pub struct Location {
    pub path: String,
    pub line: u32,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct LocationWithColumn {
    pub path: String,
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct AncestorEntry {
    pub name: String,
    pub kind: &'static str,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct MemberEntry {
    pub name: String,
    pub kind: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<Location>,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct DefinitionEntry {
    pub path: String,
    pub line: u32,
    pub comments: Vec<String>,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct SearchEntry {
    pub name: String,
    pub kind: &'static str,
    pub locations: Vec<Location>,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct SearchDeclarationsResult {
    pub results: Vec<SearchEntry>,
    pub total: usize,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct DeclarationDetail {
    pub name: String,
    pub kind: &'static str,
    pub definitions: Vec<DefinitionEntry>,
    pub ancestors: Vec<AncestorEntry>,
    pub members: Vec<MemberEntry>,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct DescendantEntry {
    pub name: String,
    pub kind: &'static str,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct DescendantsResult {
    pub name: String,
    pub descendants: Vec<DescendantEntry>,
    pub total: usize,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct ReferencesResult {
    pub name: String,
    pub references: Vec<LocationWithColumn>,
    pub total: usize,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct FileDeclarationEntry {
    pub name: String,
    pub kind: &'static str,
    pub line: u32,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct FileDeclarationsResult {
    pub file: String,
    pub declarations: Vec<FileDeclarationEntry>,
}

#[derive(Debug, Serialize, PartialEq)]
pub struct CodebaseStats {
    pub files: usize,
    pub declarations: usize,
    pub definitions: usize,
    pub constant_references: usize,
    pub method_references: usize,
    pub breakdown_by_kind: HashMap<&'static str, usize>,
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn uri_to_path(uri: &str) -> Option<PathBuf> {
    Url::parse(uri).ok()?.to_file_path().ok()
}

pub fn format_path(uri: &str, root: &Path) -> String {
    let Some(path) = uri_to_path(uri) else {
        return uri.to_string();
    };
    path.strip_prefix(root)
        .map_or_else(|_| path.display().to_string(), |rel| rel.display().to_string())
}

fn lookup_declaration<'g>(
    graph: &'g Graph,
    name: &str,
) -> Result<(DeclarationId, &'g Declaration), QueryError> {
    let declaration_id = DeclarationId::from(name);
    match graph.declarations().get(&declaration_id) {
        Some(decl) => Ok((declaration_id, decl)),
        None => Err(QueryError::NotFound {
            name: name.to_string(),
        }),
    }
}

fn require_namespace<'a>(
    decl: &'a Declaration,
    name: &str,
    tool_name: &'static str,
) -> Result<&'a Namespace, QueryError> {
    decl.as_namespace().ok_or_else(|| QueryError::InvalidKind {
        name: name.to_string(),
        actual_kind: decl.kind().to_string(),
        tool_name,
    })
}

fn collect_ancestors(graph: &Graph, ancestors: &Ancestors) -> Vec<AncestorEntry> {
    ancestors
        .iter()
        .filter_map(|ancestor| match ancestor {
            Ancestor::Complete(id) => {
                let decl = graph.declarations().get(id)?;
                Some(AncestorEntry {
                    name: decl.name().to_string(),
                    kind: decl.kind(),
                })
            }
            Ancestor::Partial(name_id) => {
                let name_ref = graph.names().get(name_id)?;
                Some(AncestorEntry {
                    name: format!("{name_ref:?}"),
                    kind: "Unresolved",
                })
            }
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Query functions
// ---------------------------------------------------------------------------

pub fn query_search_declarations(
    graph: &Graph,
    root: &Path,
    query: &str,
    kind: Option<&str>,
    limit: usize,
    offset: usize,
) -> SearchDeclarationsResult {
    let ids = rubydex::query::declaration_search(graph, query);

    let matches_filter = |id: &&_| -> bool {
        let Some(decl) = graph.declarations().get(id) else {
            return false;
        };
        if let Some(kind) = kind {
            decl.kind().eq_ignore_ascii_case(kind)
        } else {
            true
        }
    };

    let total = ids.iter().filter(matches_filter).count();
    let results = ids
        .iter()
        .filter(matches_filter)
        .skip(offset)
        .take(limit)
        .filter_map(|id| {
            let decl = graph.declarations().get(id)?;
            let locations = decl
                .definitions()
                .iter()
                .filter_map(|def_id| {
                    let def = graph.definitions().get(def_id)?;
                    let doc = graph.documents().get(def.uri_id())?;
                    let loc = def.offset().to_location(doc).to_presentation();
                    Some(Location {
                        path: format_path(doc.uri(), root),
                        line: loc.start_line(),
                    })
                })
                .collect();
            Some(SearchEntry {
                name: decl.name().to_string(),
                kind: decl.kind(),
                locations,
            })
        })
        .collect();

    SearchDeclarationsResult { results, total }
}

pub fn query_get_declaration(
    graph: &Graph,
    root: &Path,
    name: &str,
) -> Result<DeclarationDetail, QueryError> {
    let (_, decl) = lookup_declaration(graph, name)?;

    let definitions = decl
        .definitions()
        .iter()
        .filter_map(|def_id| {
            let def = graph.definitions().get(def_id)?;
            let doc = graph.documents().get(def.uri_id())?;
            let loc = def.offset().to_location(doc).to_presentation();
            let comments = def
                .comments()
                .iter()
                .map(|c| {
                    c.string()
                        .as_str()
                        .strip_prefix("# ")
                        .unwrap_or(c.string().as_str())
                        .to_string()
                })
                .collect();
            Some(DefinitionEntry {
                path: format_path(doc.uri(), root),
                line: loc.start_line(),
                comments,
            })
        })
        .collect();

    let namespace = decl.as_namespace();
    let ancestors = namespace
        .map(|ns| collect_ancestors(graph, ns.ancestors()))
        .unwrap_or_default();

    let members = namespace
        .map(|ns| {
            ns.members()
                .iter()
                .filter_map(|(_, member_id)| {
                    let member_decl = graph.declarations().get(member_id)?;
                    let member_def = member_decl
                        .definitions()
                        .first()
                        .and_then(|def_id| graph.definitions().get(def_id));

                    let location = if let Some(def) = member_def
                        && let Some(doc) = graph.documents().get(def.uri_id())
                    {
                        let loc = def.offset().to_location(doc).to_presentation();
                        Some(Location {
                            path: format_path(doc.uri(), root),
                            line: loc.start_line(),
                        })
                    } else {
                        None
                    };

                    Some(MemberEntry {
                        name: member_decl.name().to_string(),
                        kind: member_decl.kind(),
                        location,
                    })
                })
                .collect()
        })
        .unwrap_or_default();

    Ok(DeclarationDetail {
        name: decl.name().to_string(),
        kind: decl.kind(),
        definitions,
        ancestors,
        members,
    })
}

pub fn query_get_descendants(
    graph: &Graph,
    name: &str,
    limit: usize,
    offset: usize,
) -> Result<DescendantsResult, QueryError> {
    let (_, decl) = lookup_declaration(graph, name)?;
    let namespace = require_namespace(decl, name, "get_descendants")?;

    let exists = |id: &&_| graph.declarations().get(id).is_some();

    let total = namespace.descendants().iter().filter(exists).count();
    let descendants = namespace
        .descendants()
        .iter()
        .filter(exists)
        .skip(offset)
        .take(limit)
        .filter_map(|id| {
            let desc_decl = graph.declarations().get(id)?;
            Some(DescendantEntry {
                name: desc_decl.name().to_string(),
                kind: desc_decl.kind(),
            })
        })
        .collect();

    Ok(DescendantsResult {
        name: decl.name().to_string(),
        descendants,
        total,
    })
}

pub fn query_find_constant_references(
    graph: &Graph,
    root: &Path,
    name: &str,
    limit: usize,
    offset: usize,
) -> Result<ReferencesResult, QueryError> {
    let (_, decl) = lookup_declaration(graph, name)?;

    let has_document = |ref_id: &&_| {
        graph
            .constant_references()
            .get(ref_id)
            .and_then(|r| graph.documents().get(&r.uri_id()))
            .is_some()
    };

    let total = decl.references().iter().filter(has_document).count();
    let references = decl
        .references()
        .iter()
        .filter(has_document)
        .skip(offset)
        .take(limit)
        .filter_map(|ref_id| {
            let const_ref = graph.constant_references().get(ref_id)?;
            let doc = graph.documents().get(&const_ref.uri_id())?;
            let loc = const_ref.offset().to_location(doc).to_presentation();
            Some(LocationWithColumn {
                path: format_path(doc.uri(), root),
                line: loc.start_line(),
                column: loc.start_col(),
            })
        })
        .collect();

    Ok(ReferencesResult {
        name: name.to_string(),
        references,
        total,
    })
}

pub fn query_get_file_declarations(
    graph: &Graph,
    root: &Path,
    canonical_path: &Path,
) -> Result<FileDeclarationsResult, QueryError> {
    let Ok(uri) = Url::from_file_path(canonical_path) else {
        return Err(QueryError::InvalidPath {
            path: canonical_path.display().to_string(),
        });
    };

    let uri_id = UriId::from(uri.as_str());
    let Some(doc) = graph.documents().get(&uri_id) else {
        return Err(QueryError::NotFound {
            name: canonical_path.display().to_string(),
        });
    };

    let declarations = doc
        .definitions()
        .iter()
        .filter_map(|def_id| {
            let def = graph.definitions().get(def_id)?;
            let loc = def.offset().to_location(doc).to_presentation();
            let (name, kind) = graph
                .definition_id_to_declaration_id(*def_id)
                .and_then(|decl_id| graph.declarations().get(decl_id))
                .map(|decl| (decl.name().to_string(), decl.kind()))?;
            Some(FileDeclarationEntry { name, kind, line: loc.start_line() })
        })
        .collect();

    Ok(FileDeclarationsResult {
        file: format_path(doc.uri(), root),
        declarations,
    })
}

pub fn query_codebase_stats(graph: &Graph) -> CodebaseStats {
    let mut breakdown_by_kind: HashMap<&'static str, usize> = HashMap::new();
    for decl in graph.declarations().values() {
        *breakdown_by_kind.entry(decl.kind()).or_default() += 1;
    }

    CodebaseStats {
        files: graph.documents().len(),
        declarations: graph.declarations().len(),
        definitions: graph.definitions().len(),
        constant_references: graph.constant_references().len(),
        method_references: graph.method_references().len(),
        breakdown_by_kind,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use rubydex::test_utils::GraphTest;
    use std::path::PathBuf;

    fn test_root() -> PathBuf {
        if cfg!(windows) {
            PathBuf::from("C:\\test")
        } else {
            PathBuf::from("/test")
        }
    }

    fn test_uri(filename: &str) -> String {
        if cfg!(windows) {
            format!("file:///C:/test/{filename}")
        } else {
            format!("file:///test/{filename}")
        }
    }

    fn graph_with_source(source: &str) -> Graph {
        graph_with_sources(&[(&test_uri("test.rb"), source)])
    }

    fn graph_with_sources(sources: &[(&str, &str)]) -> Graph {
        let mut gt = GraphTest::new();
        for (uri, source) in sources {
            gt.index_uri(uri, source);
        }
        gt.resolve();
        gt.into_graph()
    }

    // -- query_search_declarations --

    #[test]
    fn search_returns_matching_results() {
        let graph = graph_with_source("class Dog; end");
        let root = test_root();
        let result = query_search_declarations(&graph, &root, "Dog", None, 50, 0);

        assert_eq!(result.total, 1);
        assert_eq!(result.results[0].name, "Dog");
        assert_eq!(result.results[0].kind, "Class");
        assert!(result.results[0].locations[0].path.ends_with("test.rb"));
        assert_eq!(result.results[0].locations[0].line, 1);
    }

    #[test]
    fn search_kind_filter() {
        let graph = graph_with_source(
            "
            class Dog; end
            module Walkable; end
            ",
        );
        let root = test_root();

        let result = query_search_declarations(&graph, &root, "Dog", Some("Class"), 50, 0);
        assert_eq!(result.results.len(), 1);
        assert_eq!(result.results[0].name, "Dog");

        let result = query_search_declarations(&graph, &root, "Dog", Some("Module"), 50, 0);
        assert!(result.results.is_empty());

        // Case-insensitive kind filter
        let result = query_search_declarations(&graph, &root, "Dog", Some("class"), 50, 0);
        assert_eq!(result.results.len(), 1);

        // Case-insensitive query
        let result = query_search_declarations(&graph, &root, "dog", None, 50, 0);
        assert!(!result.results.is_empty());
    }

    #[test]
    fn search_no_match() {
        let graph = graph_with_source("class Dog; end");
        let root = test_root();
        let result = query_search_declarations(&graph, &root, "Zzzzzzzzz", None, 50, 0);
        assert!(result.results.is_empty());
        assert_eq!(result.total, 0);
    }

    #[test]
    fn search_pagination() {
        let graph = graph_with_source(
            "
            class A; end
            class B; end
            class C; end
            ",
        );
        let root = test_root();

        let result = query_search_declarations(&graph, &root, "", None, 2, 0);
        assert_eq!(result.results.len(), 2);
        let total = result.total;

        let result = query_search_declarations(&graph, &root, "", None, 2, 9999);
        assert!(result.results.is_empty());
        assert_eq!(result.total, total);

        // Consecutive pages return different items
        let page1 = query_search_declarations(&graph, &root, "", None, 1, 0);
        let page2 = query_search_declarations(&graph, &root, "", None, 1, 1);
        assert_ne!(page1.results[0].name, page2.results[0].name);
    }

    // -- query_get_declaration --

    #[test]
    fn get_declaration_class_with_ancestors_and_members() {
        let graph = graph_with_source(
            "
            class Animal; end
            class Dog < Animal
              def speak; end
              def fetch; end
            end
            ",
        );
        let root = test_root();
        let result = query_get_declaration(&graph, &root, "Dog").unwrap();

        assert_eq!(result.name, "Dog");
        assert_eq!(result.kind, "Class");
        assert!(!result.definitions.is_empty());
        assert!(result.ancestors.iter().any(|a| a.name == "Animal"));
        assert!(result.members.iter().any(|m| m.name == "Dog#speak()"));
        assert!(result.members.iter().any(|m| m.name == "Dog#fetch()"));

        let speak = result.members.iter().find(|m| m.name == "Dog#speak()").unwrap();
        assert_eq!(speak.kind, "Method");
        assert!(speak.location.as_ref().unwrap().path.ends_with("test.rb"));
        assert_eq!(speak.location.as_ref().unwrap().line, 3);
    }

    #[test]
    fn get_declaration_module() {
        let graph = graph_with_source("module Greetable; end");
        let root = test_root();
        let result = query_get_declaration(&graph, &root, "Greetable").unwrap();
        assert_eq!(result.kind, "Module");
    }

    #[test]
    fn get_declaration_doc_comments() {
        let graph = graph_with_source(
            "
            # The Animal class represents all animals.
            class Animal; end
            ",
        );
        let root = test_root();
        let result = query_get_declaration(&graph, &root, "Animal").unwrap();
        assert!(result.definitions[0].comments.iter().any(|c| c.contains("Animal")));
    }

    #[test]
    fn get_declaration_mixin_ancestors() {
        let graph = graph_with_source(
            "
            module Greetable; end
            class Person
              include Greetable
            end
            ",
        );
        let root = test_root();
        let result = query_get_declaration(&graph, &root, "Person").unwrap();
        assert!(result.ancestors.iter().any(|a| a.name == "Greetable"));
    }

    #[test]
    fn get_declaration_constant() {
        let graph = graph_with_source(
            "
            class Animal
              KINGDOM = 'Animalia'
            end
            ",
        );
        let root = test_root();
        let result = query_get_declaration(&graph, &root, "Animal::KINGDOM").unwrap();
        assert_eq!(result.kind, "Constant");
        assert!(result.ancestors.is_empty());
        assert!(result.members.is_empty());
    }

    #[test]
    fn get_declaration_not_found() {
        let graph = graph_with_source("class Dog; end");
        let root = test_root();
        let err = query_get_declaration(&graph, &root, "DoesNotExist").unwrap_err();
        assert!(matches!(err, QueryError::NotFound { .. }));
    }

    // -- query_get_descendants --

    #[test]
    fn get_descendants_with_subclasses() {
        let graph = graph_with_source(
            "
            class Animal; end
            class Dog < Animal; end
            class Cat < Animal; end
            ",
        );

        let result = query_get_descendants(&graph, "Animal", 50, 0).unwrap();
        assert_eq!(result.name, "Animal");
        assert!(result.descendants.iter().any(|d| d.name == "Animal"));
        assert!(result.descendants.iter().any(|d| d.name == "Dog"));
        assert!(result.descendants.iter().any(|d| d.name == "Cat"));
        assert_eq!(result.total, 3);

        let result = query_get_descendants(&graph, "Cat", 50, 0).unwrap();
        assert_eq!(result.total, 1);
    }

    #[test]
    fn get_descendants_module() {
        let graph = graph_with_source(
            "
            module Greetable; end
            class Person
              include Greetable
            end
            ",
        );
        let result = query_get_descendants(&graph, "Greetable", 50, 0).unwrap();
        assert!(result.descendants.iter().any(|d| d.name == "Person"));
    }

    #[test]
    fn get_descendants_inheritance_chain() {
        let graph = graph_with_source(
            "
            class Foo; end
            class Bar < Foo; end
            class Baz < Bar; end
            ",
        );
        let result = query_get_descendants(&graph, "Foo", 50, 0).unwrap();
        assert!(result.descendants.iter().any(|d| d.name == "Bar"));
        assert!(result.descendants.iter().any(|d| d.name == "Baz"));
    }

    #[test]
    fn get_descendants_pagination() {
        let graph = graph_with_source(
            "
            class Animal; end
            class Dog < Animal; end
            class Cat < Animal; end
            ",
        );

        let page1 = query_get_descendants(&graph, "Animal", 1, 0).unwrap();
        assert_eq!(page1.descendants.len(), 1);
        assert_eq!(page1.total, 3);

        let page2 = query_get_descendants(&graph, "Animal", 1, 1).unwrap();
        assert_ne!(page1.descendants[0].name, page2.descendants[0].name);
    }

    #[test]
    fn get_descendants_not_found() {
        let graph = graph_with_source("class Dog; end");
        let err = query_get_descendants(&graph, "DoesNotExist", 50, 0).unwrap_err();
        assert!(matches!(err, QueryError::NotFound { .. }));
    }

    #[test]
    fn get_descendants_invalid_kind() {
        let graph = graph_with_source(
            "
            class Animal
              KINGDOM = 'Animalia'
            end
            ",
        );
        let err = query_get_descendants(&graph, "Animal::KINGDOM", 50, 0).unwrap_err();
        assert!(matches!(err, QueryError::InvalidKind { .. }));
    }

    // -- query_find_constant_references --

    #[test]
    fn find_references_success() {
        let graph = graph_with_source(
            "
            class Animal; end
            class Dog < Animal; end
            class Kennel
              def build
                Animal.new
              end
            end
            ",
        );
        let root = test_root();
        let result = query_find_constant_references(&graph, &root, "Animal", 50, 0).unwrap();

        assert_eq!(result.name, "Animal");
        assert_eq!(result.references.len(), 2);
        assert_eq!(result.total, 2);
        assert!(result.references[0].path.ends_with("test.rb"));
        assert_eq!(result.references[0].line, 2);
        assert_eq!(result.references[0].column, 13);
    }

    #[test]
    fn find_references_cross_file() {
        let models = test_uri("models.rb");
        let services = test_uri("services.rb");
        let graph = graph_with_sources(&[
            (&models, "class Dog; end"),
            (
                &services,
                "
                class Kennel
                  def adopt
                    Dog.new
                  end
                end
                ",
            ),
        ]);
        let root = test_root();
        let result = query_find_constant_references(&graph, &root, "Dog", 50, 0).unwrap();
        assert!(result.references.iter().any(|r| r.path.contains("services")));
    }

    #[test]
    fn find_references_pagination() {
        let graph = graph_with_source(
            "
            class Animal; end
            class Dog < Animal; end
            class Cat < Animal; end
            class Kennel
              def build
                Animal.new
              end
            end
            ",
        );
        let root = test_root();

        let full = query_find_constant_references(&graph, &root, "Animal", 50, 0).unwrap();
        let page = query_find_constant_references(&graph, &root, "Animal", 1, 0).unwrap();
        assert_eq!(page.references.len(), 1);
        assert_eq!(page.total, full.total);
    }

    #[test]
    fn find_references_not_found() {
        let graph = graph_with_source("class Dog; end");
        let root = test_root();
        let err = query_find_constant_references(&graph, &root, "DoesNotExist", 50, 0).unwrap_err();
        assert!(matches!(err, QueryError::NotFound { .. }));
    }

    // -- query_get_file_declarations --

    #[test]
    fn file_declarations_success() {
        let graph = graph_with_source(
            "
            class Animal; end
            class Dog < Animal; end
            module Greetable; end
            ",
        );
        let root = test_root();
        let result = query_get_file_declarations(&graph, &root, &root.join("test.rb")).unwrap();

        assert!(result.declarations.iter().any(|d| d.name == "Animal"));
        assert!(result.declarations.iter().any(|d| d.name == "Dog"));
        assert!(result.declarations.iter().any(|d| d.name == "Greetable"));
        assert_eq!(result.declarations[0].name, "Animal");
        assert_eq!(result.declarations[0].kind, "Class");
        assert_eq!(result.declarations[0].line, 1);
    }

    #[test]
    fn file_declarations_multiple_files() {
        let models = test_uri("models.rb");
        let services = test_uri("services.rb");
        let graph = graph_with_sources(&[
            (&models, "class Animal; end"),
            (&services, "class Kennel; end"),
        ]);
        let root = test_root();
        let result = query_get_file_declarations(&graph, &root, &root.join("services.rb")).unwrap();
        assert!(result.declarations.iter().any(|d| d.name == "Kennel"));
    }

    #[test]
    fn file_declarations_not_found() {
        let graph = graph_with_source("class Dog; end");
        let root = test_root();
        let err = query_get_file_declarations(&graph, &root, &root.join("nonexistent.rb")).unwrap_err();
        assert!(matches!(err, QueryError::NotFound { .. }));
    }

    // -- query_codebase_stats --

    #[test]
    fn codebase_stats_returns_counts() {
        let a = test_uri("a.rb");
        let b = test_uri("b.rb");
        let graph = graph_with_sources(&[(&a, "class Animal; end"), (&b, "module Greetable; end")]);
        let result = query_codebase_stats(&graph);

        assert_eq!(result.files, 2);
        assert_eq!(result.declarations, 5);
        assert_eq!(result.definitions, 2);
        assert_eq!(result.breakdown_by_kind["Class"], 4);
        assert_eq!(result.breakdown_by_kind["Module"], 1);
    }
}
