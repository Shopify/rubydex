use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use crate::tools::{
    DetectDeadConstantsParams, FindConstantReferencesParams, GetDeclarationParams, GetDescendantsParams,
    GetFileDeclarationsParams, SearchDeclarationsParams,
};
use rmcp::{
    ServerHandler,
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::{ServerCapabilities, ServerInfo},
    tool, tool_handler, tool_router,
    transport::io::stdio,
};
use rubydex::model::ids::{DeclarationId, UriId};
use rubydex::model::{
    declaration::{Ancestor, Ancestors, Declaration},
    graph::Graph,
};
use url::Url;

struct ServerState {
    graph: Option<Graph>,
    error: Option<String>,
}

pub struct RubydexServer {
    state: Arc<RwLock<ServerState>>,
    root_path: PathBuf,
    tool_router: ToolRouter<Self>,
}

impl RubydexServer {
    pub fn new(root: String) -> Self {
        Self {
            state: Arc::new(RwLock::new(ServerState {
                graph: None,
                error: None,
            })),
            root_path: PathBuf::from(root),
            tool_router: Self::tool_router(),
        }
    }

    /// Spawns a background thread that indexes the codebase and marks the server as ready.
    pub fn spawn_indexer(&self, path: String) {
        let state = Arc::clone(&self.state);
        std::thread::spawn(move || {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let (file_paths, errors) = rubydex::listing::collect_file_paths(vec![path]);
                for error in &errors {
                    eprintln!("Listing error: {error}");
                }

                let mut graph = Graph::new();
                let errors = rubydex::indexing::index_files(&mut graph, file_paths);
                for error in &errors {
                    eprintln!("Indexing error: {error}");
                }

                let mut resolver = rubydex::resolution::Resolver::new(&mut graph);
                resolver.resolve_all();

                eprintln!(
                    "Rubydex indexed {} files, {} declarations",
                    graph.documents().len(),
                    graph.declarations().len()
                );
                graph
            }));

            let mut state = state.write().expect("state lock poisoned");
            match result {
                Ok(graph) => {
                    state.graph = Some(graph);
                }
                Err(panic) => {
                    let msg = panic
                        .downcast_ref::<String>()
                        .map(String::as_str)
                        .or_else(|| panic.downcast_ref::<&str>().copied())
                        .unwrap_or("unknown error");
                    eprintln!("Rubydex indexing failed: {msg}");
                    state.error = Some(msg.to_string());
                }
            }
        });
    }

    pub async fn serve(self) -> Result<(), Box<dyn std::error::Error>> {
        let service = rmcp::ServiceExt::serve(self, stdio()).await?;
        service.waiting().await?;
        Ok(())
    }
}

/// Returns a structured JSON error string with a machine-readable type, message, and suggestion.
fn error_json(error_type: &str, message: &str, suggestion: &str) -> String {
    serde_json::to_string(&serde_json::json!({
        "error": error_type,
        "message": message,
        "suggestion": suggestion,
    }))
    .unwrap_or_else(|_| "{}".to_string())
}

/// Acquires the read lock and returns a guard with the graph if ready.
/// Returns early with a JSON error if still indexing or if indexing failed.
macro_rules! ensure_graph_ready {
    ($self:expr) => {{
        let state = $self.state.read().expect("state lock poisoned");
        if let Some(err) = &state.error {
            return error_json(
                "indexing_failed",
                &format!("Rubydex indexing failed: {err}"),
                "Check server logs for details. The MCP server needs to be restarted.",
            );
        }
        if state.graph.is_none() {
            return error_json(
                "indexing",
                "Rubydex is still indexing the codebase",
                "The server is starting up. Please retry in a few seconds.",
            );
        }
        state
    }};
}

/// Looks up a declaration by name, returning an error JSON string from the caller if not found.
macro_rules! lookup_declaration {
    ($graph:expr, $name:expr) => {{
        let declaration_id = DeclarationId::from($name);
        match $graph.declarations().get(&declaration_id) {
            Some(decl) => (declaration_id, decl),
            None => {
                return error_json(
                    "not_found",
                    &format!("Declaration '{}' not found", $name),
                    "Try search_declarations with a partial name to find the correct FQN",
                );
            }
        }
    }};
}

/// Narrows a declaration to a namespace, returning an error JSON string if it's not a class or module.
macro_rules! require_namespace {
    ($decl:expr, $name:expr, $tool_name:literal) => {
        match $decl.as_namespace() {
            Some(ns) => ns,
            None => {
                return error_json(
                    "invalid_kind",
                    &format!("'{}' is not a class or module (it is a {})", $name, $decl.kind()),
                    concat!(
                        $tool_name,
                        " only works on classes and modules, not methods or constants"
                    ),
                );
            }
        }
    };
}

/// Parses a file URI into a platform-native absolute path.
fn uri_to_path(uri: &str) -> Option<PathBuf> {
    Url::parse(uri).ok()?.to_file_path().ok()
}

/// Converts a file URI to a path relative to `root` when possible.
/// Falls back to an absolute display path if it cannot be relativized.
fn format_path(uri: &str, root: &Path) -> String {
    let Some(path) = uri_to_path(uri) else {
        return uri.to_string();
    };

    path.strip_prefix(root)
        .map_or_else(|_| path.display().to_string(), |rel| rel.display().to_string())
}

/// Returns true for declarations that are part of Ruby's built-in runtime and should never
/// be reported as dead code.
fn is_infrastructure_declaration(name: &str) -> bool {
    matches!(name, "Object" | "BasicObject" | "Module" | "Class")
}

/// Formats an ancestor chain into a JSON array of `{"name": ..., "kind": ...}` objects.
fn format_ancestors(graph: &Graph, ancestors: &Ancestors) -> Vec<serde_json::Value> {
    ancestors
        .iter()
        .filter_map(|ancestor| match ancestor {
            Ancestor::Complete(id) => {
                let ancestor_decl = graph.declarations().get(id)?;
                Some(serde_json::json!({
                    "name": ancestor_decl.name(),
                    "kind": ancestor_decl.kind(),
                }))
            }
            Ancestor::Partial(name_id) => {
                let name_ref = graph.names().get(name_id)?;
                Some(serde_json::json!({
                    "name": format!("{name_ref:?}"),
                    "kind": "Unresolved",
                }))
            }
        })
        .collect()
}

/// Filters, paginates, and maps items. Returns `(results, total)` where `total` is the
/// count of all items passing the filter, and `results` contains only the requested page.
macro_rules! paginate {
    ($items:expr, $offset:expr, $limit:expr, $filter:expr, $map:expr $(,)?) => {{
        let filtered: Vec<_> = $items.filter($filter).collect();
        let total = filtered.len();
        let results: Vec<serde_json::Value> = filtered
            .into_iter()
            .skip($offset)
            .take($limit)
            .filter_map($map)
            .collect();
        (results, total)
    }};
}

#[tool_router]
impl RubydexServer {
    #[tool(
        description = "Search for Ruby classes, modules, methods, or constants by name. Use this INSTEAD OF Grep when you know part of a Ruby identifier name and want to find its definition. Returns fully qualified names, kinds, and file locations. Use the `kind` filter (\"Class\", \"Module\", \"Method\", \"Constant\") to narrow results. Results are paginated: the response includes `total` (the full count of matches). If `total` exceeds the number of returned results, use `offset` to fetch subsequent pages."
    )]
    fn search_declarations(&self, Parameters(params): Parameters<SearchDeclarationsParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();
        let ids = rubydex::query::declaration_search(graph, &params.query);

        let limit = params.limit.filter(|&l| l > 0).unwrap_or(25).min(100); // default 25, max 100
        let offset = params.offset.unwrap_or(0);
        let kind_filter = params.kind.as_deref();

        let (results, total) = paginate!(
            ids.iter(),
            offset,
            limit,
            |id| {
                let Some(decl) = graph.declarations().get(id) else {
                    return false;
                };
                if let Some(kind) = kind_filter {
                    decl.kind().eq_ignore_ascii_case(kind)
                } else {
                    true
                }
            },
            |id| {
                let decl = graph.declarations().get(id)?;
                let locations: Vec<serde_json::Value> = decl
                    .definitions()
                    .iter()
                    .filter_map(|def_id| {
                        let def = graph.definitions().get(def_id)?;
                        let doc = graph.documents().get(def.uri_id())?;
                        let loc = def.offset().to_location(doc).to_presentation();
                        Some(serde_json::json!({
                            "path": format_path(doc.uri(), &self.root_path),
                            "line": loc.start_line(),
                        }))
                    })
                    .collect();

                Some(serde_json::json!({
                    "name": decl.name(),
                    "kind": decl.kind(),
                    "locations": locations,
                }))
            },
        );

        let result = serde_json::json!({
            "results": results,
            "total": total,
        });

        serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string())
    }

    #[tool(
        description = "Get complete information about a Ruby class, module, method, or constant by its exact fully qualified name. Returns file locations, documentation comments, ancestor chain, and members with locations. FQN format: \"Foo::Bar\" for classes/modules/constants, \"Foo::Bar#method_name\" for instance methods."
    )]
    fn get_declaration(&self, Parameters(params): Parameters<GetDeclarationParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();
        let (_, decl) = lookup_declaration!(graph, &params.name);

        let definitions: Vec<serde_json::Value> = decl
            .definitions()
            .iter()
            .filter_map(|def_id| {
                let def = graph.definitions().get(def_id)?;
                let doc = graph.documents().get(def.uri_id())?;
                let loc = def.offset().to_location(doc).to_presentation();
                let path = format_path(doc.uri(), &self.root_path);
                let comments: Vec<String> = def
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

                Some(serde_json::json!({
                    "path": path,
                    "line": loc.start_line(),
                    "comments": comments,
                }))
            })
            .collect();

        let namespace = decl.as_namespace();
        let ancestors = namespace
            .map(|ns| format_ancestors(graph, ns.ancestors()))
            .unwrap_or_default();

        let members: Vec<serde_json::Value> = namespace
            .map(|ns| {
                ns.members()
                    .iter()
                    .filter_map(|(_, member_id)| {
                        let member_decl = graph.declarations().get(member_id)?;
                        let member_def = member_decl
                            .definitions()
                            .first()
                            .and_then(|def_id| graph.definitions().get(def_id));

                        let mut member = serde_json::json!({
                            "name": member_decl.name(),
                            "kind": member_decl.kind(),
                        });

                        if let Some(def) = member_def
                            && let Some(doc) = graph.documents().get(def.uri_id())
                        {
                            let loc = def.offset().to_location(doc).to_presentation();
                            member["location"] = serde_json::json!({
                                "path": format_path(doc.uri(), &self.root_path),
                                "line": loc.start_line(),
                            });
                        }

                        Some(member)
                    })
                    .collect()
            })
            .unwrap_or_default();

        let result = serde_json::json!({
            "name": decl.name(),
            "kind": decl.kind(),
            "definitions": definitions,
            "ancestors": ancestors,
            "members": members,
        });

        serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string())
    }

    #[tool(
        description = "Find all classes and modules that inherit from or include a given class/module. Returns known descendants (including itself and transitive relationships) for impact analysis before modifying a base class or module. Results are paginated: the response includes `total`. If `total` exceeds the number of returned results, use `offset` to fetch subsequent pages."
    )]
    fn get_descendants(&self, Parameters(params): Parameters<GetDescendantsParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();
        let (_, decl) = lookup_declaration!(graph, &params.name);
        let namespace = require_namespace!(decl, &params.name, "get_descendants");

        let limit = params.limit.filter(|&l| l > 0).unwrap_or(100).min(500); // default 100, max 500
        let offset = params.offset.unwrap_or(0);

        let (descendants, total) = paginate!(
            namespace.descendants().iter(),
            offset,
            limit,
            |id| graph.declarations().get(id).is_some(),
            |id| {
                let desc_decl = graph.declarations().get(id)?;
                Some(serde_json::json!({
                    "name": desc_decl.name(),
                    "kind": desc_decl.kind(),
                }))
            },
        );

        let result = serde_json::json!({
            "name": decl.name(),
            "descendants": descendants,
            "total": total,
        });

        serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string())
    }

    #[tool(
        description = "Find all resolved references to a Ruby class, module, or constant across the codebase. Returns file paths, line numbers, and columns for each usage. Results are paginated: the response includes `total`. If `total` exceeds the number of returned results, use `offset` to fetch subsequent pages."
    )]
    fn find_constant_references(&self, Parameters(params): Parameters<FindConstantReferencesParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();
        let (_, decl) = lookup_declaration!(graph, &params.name);

        let limit = params.limit.filter(|&l| l > 0).unwrap_or(50).min(200); // default 50, max 200
        let offset = params.offset.unwrap_or(0);

        let (references, total) = paginate!(
            decl.references().iter(),
            offset,
            limit,
            |ref_id| {
                graph
                    .constant_references()
                    .get(ref_id)
                    .and_then(|r| graph.documents().get(&r.uri_id()))
                    .is_some()
            },
            |ref_id| {
                let const_ref = graph.constant_references().get(ref_id)?;
                let doc = graph.documents().get(&const_ref.uri_id())?;
                let loc = const_ref.offset().to_location(doc).to_presentation();
                Some(serde_json::json!({
                    "path": format_path(doc.uri(), &self.root_path),
                    "line": loc.start_line(),
                    "column": loc.start_col(),
                }))
            },
        );

        let result = serde_json::json!({
            "name": params.name,
            "references": references,
            "total": total,
        });

        serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string())
    }

    #[tool(
        description = "Find Ruby classes, modules, and constants with zero resolved references across the codebase — dead code candidates. Returns declarations that are defined but never referenced anywhere, with file locations and owning namespace. Uses resolved name references (not string matching) for high accuracy. Verify critical results with find_constant_references before deletion. Results are paginated: the response includes `total`."
    )]
    fn detect_dead_constants(&self, Parameters(params): Parameters<DetectDeadConstantsParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();

        let limit = params.limit.filter(|&l| l > 0).unwrap_or(50).min(200); // default 50, max 200
        let offset = params.offset.unwrap_or(0);
        let kind_filter = params.kind.as_deref();

        // Resolve optional file_path to an absolute prefix for matching
        let path_prefix = params.file_path.as_ref().map(|fp| {
            let p = if Path::new(fp).is_absolute() {
                PathBuf::from(fp)
            } else {
                self.root_path.join(fp)
            };
            std::fs::canonicalize(&p).unwrap_or(p)
        });

        // Collect and sort by name for deterministic pagination order.
        // Cannot use paginate! here because graph.declarations() is a HashMap
        // with non-deterministic iteration order.
        let mut filtered: Vec<_> = graph
            .declarations()
            .values()
            .filter(|decl| {
                let kind = decl.kind();
                if !matches!(kind, "Class" | "Module" | "Constant" | "ConstantAlias") {
                    return false;
                }

                if let Some(filter) = kind_filter
                    && !kind.eq_ignore_ascii_case(filter)
                {
                    return false;
                }

                if !decl.references().is_empty() {
                    return false;
                }

                if is_infrastructure_declaration(decl.name()) {
                    return false;
                }

                if let Some(prefix) = &path_prefix {
                    let has_matching_def = decl.definitions().iter().any(|def_id| {
                        graph
                            .definitions()
                            .get(def_id)
                            .and_then(|def| graph.documents().get(def.uri_id()))
                            .and_then(|doc| uri_to_path(doc.uri()))
                            .is_some_and(|p| p.starts_with(prefix))
                    });
                    if !has_matching_def {
                        return false;
                    }
                }

                true
            })
            .collect();
        filtered.sort_by_key(|decl| decl.name());
        let total = filtered.len();

        let results: Vec<serde_json::Value> = filtered
            .into_iter()
            .skip(offset)
            .take(limit)
            .map(|decl| {
                let file_info = decl.definitions().first().and_then(|def_id| {
                    let def = graph.definitions().get(def_id)?;
                    let doc = graph.documents().get(def.uri_id())?;
                    let loc = def.offset().to_location(doc).to_presentation();
                    Some((format_path(doc.uri(), &self.root_path), loc.start_line()))
                });

                let owner_name = graph
                    .declarations()
                    .get(decl.owner_id())
                    .map_or("<unknown>", Declaration::name);

                serde_json::json!({
                    "name": decl.name(),
                    "kind": decl.kind(),
                    "file": file_info.as_ref().map(|(f, _)| f.as_str()),
                    "line": file_info.map(|(_, l)| l),
                    "owner": owner_name,
                })
            })
            .collect();

        let result = serde_json::json!({
            "results": results,
            "total": total,
        });

        serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string())
    }

    #[tool(
        description = "List all Ruby classes, modules, methods, and constants defined in a specific file. Returns a structural overview with names, kinds, and line numbers. Use this to understand a file's structure before reading it, or to see what a file contributes to the codebase. Accepts relative or absolute paths."
    )]
    fn get_file_declarations(&self, Parameters(params): Parameters<GetFileDeclarationsParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();

        let absolute_target = if Path::new(&params.file_path).is_absolute() {
            PathBuf::from(&params.file_path)
        } else {
            self.root_path.join(&params.file_path)
        };
        let canonical_target = std::fs::canonicalize(&absolute_target).unwrap_or(absolute_target);

        let Ok(uri) = Url::from_file_path(&canonical_target) else {
            return error_json(
                "invalid_path",
                &format!("Cannot convert '{}' to a file URI", params.file_path),
                "Use a relative path like 'app/models/user.rb' or an absolute path",
            );
        };

        let uri_id = UriId::from(uri.as_str());
        let Some(doc) = graph.documents().get(&uri_id) else {
            return error_json(
                "not_found",
                &format!("File '{}' not found in the index", params.file_path),
                "Use a relative path like 'app/models/user.rb' or an absolute path matching the indexed project",
            );
        };

        let mut declarations: Vec<serde_json::Value> = Vec::new();

        for def_id in doc.definitions() {
            let Some(def) = graph.definitions().get(def_id) else {
                continue;
            };

            let loc = def.offset().to_location(doc).to_presentation();

            let decl_name = graph
                .definition_id_to_declaration_id(*def_id)
                .and_then(|decl_id| graph.declarations().get(decl_id))
                .map(|decl| (decl.name().to_string(), decl.kind()));

            if let Some((name, kind)) = decl_name {
                declarations.push(serde_json::json!({
                    "name": name,
                    "kind": kind,
                    "line": loc.start_line(),
                }));
            }
        }

        let result = serde_json::json!({
            "file": format_path(doc.uri(), &self.root_path),
            "declarations": declarations,
        });

        serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string())
    }

    #[tool(
        description = "Get an overview of the indexed Ruby codebase: total file count, declaration counts, and breakdown by kind (classes, modules, methods, constants). Use this to understand codebase size and composition, or to verify that indexing completed successfully."
    )]
    fn codebase_stats(&self) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();

        let mut breakdown: HashMap<&str, usize> = HashMap::new();
        for decl in graph.declarations().values() {
            *breakdown.entry(decl.kind()).or_default() += 1;
        }

        let breakdown_json: serde_json::Value = breakdown
            .iter()
            .map(|(k, v)| (k.to_string(), serde_json::json!(v)))
            .collect();

        let result = serde_json::json!({
            "files": graph.documents().len(),
            "declarations": graph.declarations().len(),
            "definitions": graph.definitions().len(),
            "constant_references": graph.constant_references().len(),
            "method_references": graph.method_references().len(),
            "breakdown_by_kind": breakdown_json,
        });

        serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string())
    }
}

const SERVER_INSTRUCTIONS: &str = r#"Rubydex provides semantic Ruby code intelligence. Use these tools INSTEAD OF Grep when working with Ruby code structure.

Decision guide:
- Know a name? -> search_declarations (fuzzy search by name)
- Have an exact fully qualified name? -> get_declaration (full details with docs, ancestors, members)
- Need reverse hierarchy? -> get_descendants (what inherits from this class/module)
- Refactoring a class/module/constant? -> find_constant_references (all precise usages across codebase)
- Exploring a file? -> get_file_declarations (structural overview)
- Finding dead code? -> detect_dead_constants (unreferenced classes, modules, constants)
- Want general statistics? -> codebase_stats (size and composition)

Typical workflow: search_declarations -> get_declaration -> find_constant_references.

Fully qualified name format: "Foo::Bar" for classes/modules/constants, "Foo::Bar#method_name" for instance methods.

Pagination: search_declarations, get_descendants, find_constant_references, and detect_dead_constants return paginated results. The response includes a `total` count. When `total` exceeds the number of returned items, use `offset` to fetch the next page.

Use Grep instead for: literal string search, log messages, comments, non-Ruby files, or content search rather than structural queries."#;

#[tool_handler]
impl ServerHandler for RubydexServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some(SERVER_INSTRUCTIONS.into()),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rubydex::test_utils::GraphTest;
    use serde_json::Value;

    fn parse(json_str: &str) -> Value {
        serde_json::from_str(json_str).unwrap()
    }

    /// Assert a JSON array field contains an entry with the given "name".
    macro_rules! assert_includes {
        ($json:expr, $field:literal, $name:expr) => {{
            let json = &$json;
            let entries = json[$field]
                .as_array()
                .expect(concat!("expected '", $field, "' to be an array"));
            assert!(
                entries.iter().any(|e| e["name"].as_str() == Some($name)),
                "Expected '{}' in '{}', got: {:?}",
                $name,
                $field,
                entries.iter().filter_map(|e| e["name"].as_str()).collect::<Vec<_>>()
            );
        }};
    }

    /// Extract a JSON field as an array, panicking if not an array.
    macro_rules! array {
        ($json:expr, $field:literal) => {
            $json[$field]
                .as_array()
                .expect(concat!("expected '", $field, "' to be an array"))
        };
    }

    /// Assert a JSON field equals the expected u64 value.
    macro_rules! assert_json_int {
        ($json:expr, $field:literal, $val:expr) => {
            assert_eq!(
                $json[$field]
                    .as_u64()
                    .expect(concat!("expected '", $field, "' to be a number")),
                $val
            );
        };
    }

    fn assert_error(json_str: &str, expected_type: &str) {
        let res = parse(json_str);
        assert_eq!(
            res["error"].as_str(),
            Some(expected_type),
            "Expected error '{expected_type}', got: {res}"
        );
        assert!(res["message"].as_str().is_some());
        assert!(res["suggestion"].as_str().is_some());
    }

    /// Returns a platform-appropriate test root path and its file URI prefix.
    fn test_root() -> (&'static str, &'static str) {
        if cfg!(windows) {
            ("C:\\test", "file:///C:/test")
        } else {
            ("/test", "file:///test")
        }
    }

    fn test_uri(filename: &str) -> String {
        let (_, uri_prefix) = test_root();
        format!("{uri_prefix}/{filename}")
    }

    /// Build a server from a single Ruby source.
    fn server_with_source(source: &str) -> RubydexServer {
        server_with_sources(&[(&test_uri("test.rb"), source)])
    }

    /// Build a server from multiple `(uri, source)` pairs.
    fn server_with_sources(sources: &[(&str, &str)]) -> RubydexServer {
        let mut gt = GraphTest::new();
        for (uri, source) in sources {
            gt.index_uri(uri, source);
        }
        gt.resolve();

        let (root, _) = test_root();
        let server = RubydexServer::new(root.to_string());
        {
            let mut state = server.state.write().unwrap();
            state.graph = Some(gt.into_graph());
        }
        server
    }

    macro_rules! search_declarations {
        ($server:expr, $($field:ident: $val:expr),* $(,)?) => {
            parse(&$server.search_declarations(Parameters(SearchDeclarationsParams {
                $($field: $val,)*
            })))
        };
    }

    macro_rules! get_descendants {
        ($server:expr, $($field:ident: $val:expr),* $(,)?) => {
            parse(&$server.get_descendants(Parameters(GetDescendantsParams {
                $($field: $val,)*
            })))
        };
    }

    macro_rules! find_constant_references {
        ($server:expr, $($field:ident: $val:expr),* $(,)?) => {
            parse(&$server.find_constant_references(Parameters(FindConstantReferencesParams {
                $($field: $val,)*
            })))
        };
    }

    fn get_declaration(server: &RubydexServer, name: &str) -> Value {
        parse(&server.get_declaration(Parameters(GetDeclarationParams { name: name.to_string() })))
    }

    fn get_file_declarations(server: &RubydexServer, file_path: &str) -> Value {
        parse(&server.get_file_declarations(Parameters(GetFileDeclarationsParams {
            file_path: file_path.to_string(),
        })))
    }

    // -- search_declarations --

    #[test]
    fn search_declarations_returns_matching_results() {
        let s = server_with_source("class Dog; end");
        let res = search_declarations!(s, query: "Dog".into(), kind: None, limit: None, offset: None);

        assert_includes!(res, "results", "Dog");
        assert_json_int!(res, "total", 1);

        let first = &array!(res, "results")[0];
        assert_eq!(first["name"], "Dog");
        assert_eq!(first["kind"], "Class");
        assert!(first["locations"][0]["path"].as_str().unwrap().ends_with("test.rb"));
        assert_json_int!(first["locations"][0], "line", 1);
    }

    #[test]
    fn search_declarations_kind_filter() {
        let s = server_with_source(
            "
            class Dog; end
            module Walkable; end
            ",
        );

        let res = search_declarations!(s, query: "Dog".into(), kind: Some("Class".into()), limit: None, offset: None);
        assert_includes!(res, "results", "Dog");

        let res = search_declarations!(s, query: "Dog".into(), kind: Some("Module".into()), limit: None, offset: None);
        assert!(array!(res, "results").is_empty());

        // Case-insensitive
        let res = search_declarations!(s, query: "Dog".into(), kind: Some("class".into()), limit: None, offset: None);
        assert_includes!(res, "results", "Dog");

        let res = search_declarations!(s, query: "dog".into(), kind: None, limit: None, offset: None);
        assert_includes!(res, "results", "Dog");
    }

    #[test]
    fn search_declarations_no_match() {
        let s = server_with_source("class Dog; end");
        let res = search_declarations!(s, query: "Zzzzzzzzz".into(), kind: None, limit: None, offset: None);
        assert!(array!(res, "results").is_empty());
        assert_json_int!(res, "total", 0);
    }

    #[test]
    fn search_declarations_pagination() {
        let s = server_with_source(
            "
            class A; end
            class B; end
            class C; end
            ",
        );

        let res = search_declarations!(s, query: String::new(), kind: None, limit: Some(2), offset: Some(0));
        assert_eq!(array!(res, "results").len(), 2);
        let total = res["total"].as_u64().unwrap();

        let res = search_declarations!(s, query: String::new(), kind: None, limit: Some(2), offset: Some(9999));
        assert!(array!(res, "results").is_empty());
        assert_json_int!(res, "total", total);

        // Verify consecutive pages return different items
        let page1 = search_declarations!(s, query: String::new(), kind: None, limit: Some(1), offset: Some(0));
        let page2 = search_declarations!(s, query: String::new(), kind: None, limit: Some(1), offset: Some(1));
        let name1 = array!(page1, "results")[0]["name"].as_str().unwrap();
        let name2 = array!(page2, "results")[0]["name"].as_str().unwrap();
        assert_ne!(name1, name2, "Page 1 and page 2 should return different items");
    }

    // -- get_declaration --

    #[test]
    fn get_declaration_class_with_ancestors_and_members() {
        let s = server_with_source(
            "
            class Animal; end
            class Dog < Animal
              def speak; end
              def fetch; end
            end
            ",
        );
        let res = get_declaration(&s, "Dog");

        assert_eq!(res["name"], "Dog");
        assert_eq!(res["kind"], "Class");
        assert!(!array!(res, "definitions").is_empty());
        assert_includes!(res, "ancestors", "Animal");
        assert_includes!(res, "members", "Dog#speak()");
        assert_includes!(res, "members", "Dog#fetch()");

        let member = array!(res, "members")
            .iter()
            .find(|m| m["name"].as_str() == Some("Dog#speak()"))
            .unwrap();
        assert_eq!(member["kind"], "Method");
        assert!(member["location"]["path"].as_str().unwrap().ends_with("test.rb"));
        assert_json_int!(member["location"], "line", 3);
    }

    #[test]
    fn get_declaration_module() {
        let s = server_with_source("module Greetable; end");
        assert_eq!(get_declaration(&s, "Greetable")["kind"], "Module");
    }

    #[test]
    fn get_declaration_doc_comments() {
        let s = server_with_source(
            "
            # The Animal class represents all animals.
            class Animal; end
            ",
        );
        let res = get_declaration(&s, "Animal");
        let comments = array!(res["definitions"][0], "comments");
        assert!(
            comments.iter().any(|c| c.as_str().unwrap().contains("Animal")),
            "Expected doc comment on Animal, got: {comments:?}"
        );
    }

    #[test]
    fn get_declaration_mixin_ancestors() {
        let s = server_with_source(
            "
            module Greetable; end
            class Person
              include Greetable
            end
            ",
        );
        assert_includes!(get_declaration(&s, "Person"), "ancestors", "Greetable");
    }

    #[test]
    fn get_declaration_constant() {
        let s = server_with_source(
            "
            class Animal
              KINGDOM = 'Animalia'
            end
            ",
        );
        let res = get_declaration(&s, "Animal::KINGDOM");
        assert_eq!(res["kind"], "Constant");
        assert!(array!(res, "ancestors").is_empty());
        assert!(array!(res, "members").is_empty());
    }

    #[test]
    fn get_declaration_not_found() {
        let s = server_with_source("class Dog; end");
        assert_error(
            &s.get_declaration(Parameters(GetDeclarationParams {
                name: "DoesNotExist".into(),
            })),
            "not_found",
        );
    }

    // -- get_descendants --

    #[test]
    fn get_descendants_with_subclasses() {
        let s = server_with_source(
            "
            class Animal; end
            class Dog < Animal; end
            class Cat < Animal; end
            ",
        );

        let res = get_descendants!(s, name: "Animal".into(), limit: None, offset: None);
        assert_eq!(res["name"], "Animal");
        assert_includes!(res, "descendants", "Animal");
        assert_includes!(res, "descendants", "Dog");
        assert_includes!(res, "descendants", "Cat");
        assert_json_int!(res, "total", 3);

        // Cat: 1 descendant (itself only, no subclasses)
        let res = get_descendants!(s, name: "Cat".into(), limit: None, offset: None);
        assert_json_int!(res, "total", 1);
    }

    #[test]
    fn get_descendants_module() {
        let s = server_with_source(
            "
            module Greetable; end

            class Person
              include Greetable
            end
            ",
        );
        let res = get_descendants!(s, name: "Greetable".into(), limit: None, offset: None);
        assert_includes!(res, "descendants", "Person");
    }

    #[test]
    fn get_descendants_inheritance_chain() {
        let s = server_with_source(
            "
            class Foo; end
            class Bar < Foo; end
            class Baz < Bar; end
            ",
        );
        let res = get_descendants!(s, name: "Foo".into(), limit: None, offset: None);
        assert_includes!(res, "descendants", "Bar");
        assert_includes!(res, "descendants", "Baz");
    }

    #[test]
    fn get_descendants_pagination() {
        let s = server_with_source(
            "
            class Animal; end
            class Dog < Animal; end
            class Cat < Animal; end
            ",
        );
        let page1 = get_descendants!(s, name: "Animal".into(), limit: Some(1), offset: Some(0));
        assert_eq!(array!(page1, "descendants").len(), 1);
        assert_json_int!(page1, "total", 3);

        let page2 = get_descendants!(s, name: "Animal".into(), limit: Some(1), offset: Some(1));
        let name1 = array!(page1, "descendants")[0]["name"].as_str().unwrap();
        let name2 = array!(page2, "descendants")[0]["name"].as_str().unwrap();
        assert_ne!(name1, name2, "Page 1 and page 2 should return different descendants");
    }

    #[test]
    fn get_descendants_not_found() {
        let s = server_with_source("class Dog; end");
        assert_error(
            &s.get_descendants(Parameters(GetDescendantsParams {
                name: "DoesNotExist".into(),
                limit: None,
                offset: None,
            })),
            "not_found",
        );
    }

    #[test]
    fn get_descendants_invalid_kind() {
        let s = server_with_source(
            "
            class Animal
              KINGDOM = 'Animalia'
            end
            ",
        );
        assert_error(
            &s.get_descendants(Parameters(GetDescendantsParams {
                name: "Animal::KINGDOM".into(),
                limit: None,
                offset: None,
            })),
            "invalid_kind",
        );
    }

    // -- find_constant_references --

    #[test]
    fn find_constant_references_success() {
        let s = server_with_source(
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
        let res = find_constant_references!(s, name: "Animal".into(), limit: None, offset: None);

        assert_eq!(res["name"], "Animal");
        assert_eq!(array!(res, "references").len(), 2);
        assert_json_int!(res, "total", 2);
        let first_ref = &array!(res, "references")[0];
        assert!(first_ref["path"].as_str().unwrap().ends_with("test.rb"));
        assert_json_int!(first_ref, "line", 2);
        assert_json_int!(first_ref, "column", 13);
    }

    #[test]
    fn find_constant_references_cross_file() {
        let models = test_uri("models.rb");
        let services = test_uri("services.rb");
        let s = server_with_sources(&[
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
        let res = find_constant_references!(s, name: "Dog".into(), limit: None, offset: None);
        let paths: Vec<&str> = array!(res, "references")
            .iter()
            .filter_map(|r| r["path"].as_str())
            .collect();
        assert!(
            paths.iter().any(|p| p.contains("services")),
            "Expected cross-file ref from services, got: {paths:?}"
        );
    }

    #[test]
    fn find_constant_references_pagination() {
        let s = server_with_source(
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
        let full = find_constant_references!(s, name: "Animal".into(), limit: None, offset: None);
        let full_total = full["total"].as_u64().unwrap();

        let page = find_constant_references!(s, name: "Animal".into(), limit: Some(1), offset: Some(0));
        assert_eq!(array!(page, "references").len(), 1);
        assert_json_int!(page, "total", full_total);
    }

    #[test]
    fn find_constant_references_not_found() {
        let s = server_with_source("class Dog; end");
        assert_error(
            &s.find_constant_references(Parameters(FindConstantReferencesParams {
                name: "DoesNotExist".into(),
                limit: None,
                offset: None,
            })),
            "not_found",
        );
    }

    // -- detect_dead_constants --

    /// Extract all "name" strings from a JSON array field.
    fn result_names<'a>(json: &'a Value, field: &str) -> Vec<&'a str> {
        json[field]
            .as_array()
            .expect("expected array")
            .iter()
            .filter_map(|e| e["name"].as_str())
            .collect()
    }

    macro_rules! detect_dead_constants {
        ($server:expr, $($field:ident: $val:expr),* $(,)?) => {
            parse(&$server.detect_dead_constants(Parameters(DetectDeadConstantsParams {
                $($field: $val,)*
            })))
        };
    }

    #[test]
    fn detect_dead_constants_finds_unreferenced_class() {
        let s = server_with_source(
            "
            class LiveClass; end
            class DeadClass; end
            class Consumer < LiveClass; end
            ",
        );
        let res = detect_dead_constants!(s, kind: None, file_path: None, limit: None, offset: None);
        let names = result_names(&res, "results");

        assert!(names.contains(&"DeadClass"), "DeadClass has no references, got: {names:?}");
        assert!(!names.contains(&"LiveClass"), "LiveClass is referenced by Consumer, got: {names:?}");

        let entry = array!(res, "results")
            .iter()
            .find(|e| e["name"].as_str() == Some("DeadClass"))
            .unwrap();
        assert_eq!(entry["kind"], "Class");
        assert!(entry["file"].as_str().unwrap().ends_with("test.rb"));
        assert!(entry["line"].as_u64().is_some());
    }

    #[test]
    fn detect_dead_constants_superclass_counts_as_reference() {
        let s = server_with_source(
            "
            class Base; end
            class LiveChild < Base; end
            class DeadChild < Base; end
            x = LiveChild.new
            ",
        );
        let res = detect_dead_constants!(s, kind: None, file_path: None, limit: None, offset: None);
        let names = result_names(&res, "results");

        assert!(!names.contains(&"Base"), "Base is referenced as superclass, got: {names:?}");
        assert!(!names.contains(&"LiveChild"), "LiveChild is referenced by x, got: {names:?}");
        assert!(names.contains(&"DeadChild"), "DeadChild has no references, got: {names:?}");
    }

    #[test]
    fn detect_dead_constants_include_counts_as_reference() {
        let s = server_with_source(
            "
            module LiveModule; end
            module DeadModule; end
            class User
              include LiveModule
            end
            ",
        );
        let res = detect_dead_constants!(s, kind: None, file_path: None, limit: None, offset: None);
        let names = result_names(&res, "results");

        assert!(!names.contains(&"LiveModule"), "LiveModule is included by User, got: {names:?}");
        assert!(names.contains(&"DeadModule"), "DeadModule has no references, got: {names:?}");
    }

    #[test]
    fn detect_dead_constants_finds_unreferenced_constant_alias() {
        let s = server_with_source(
            "
            class Original; end
            LiveAlias = Original
            DeadAlias = Original
            x = LiveAlias.new
            ",
        );
        let res = detect_dead_constants!(s, kind: None, file_path: None, limit: None, offset: None);
        let names = result_names(&res, "results");

        assert!(names.contains(&"DeadAlias"), "DeadAlias has no references, got: {names:?}");
        assert!(!names.contains(&"LiveAlias"), "LiveAlias is referenced by x, got: {names:?}");
        assert!(!names.contains(&"Original"), "Original is referenced by aliases, got: {names:?}");
    }

    #[test]
    fn detect_dead_constants_finds_unreferenced_module() {
        let s = server_with_source(
            "
            module LiveModule; end
            module DeadModule; end
            class Foo
              include LiveModule
            end
            ",
        );
        let res = detect_dead_constants!(s, kind: None, file_path: None, limit: None, offset: None);
        let names = result_names(&res, "results");

        assert!(names.contains(&"DeadModule"), "DeadModule has no references, got: {names:?}");
        assert!(!names.contains(&"LiveModule"), "LiveModule is included by Foo, got: {names:?}");
    }

    #[test]
    fn detect_dead_constants_finds_unreferenced_constant() {
        let s = server_with_source(
            "
            class Holder
              LIVE_CONST = 1
              DEAD_CONST = 2
            end
            x = Holder::LIVE_CONST
            ",
        );
        let res = detect_dead_constants!(s, kind: None, file_path: None, limit: None, offset: None);
        let names = result_names(&res, "results");

        assert!(names.contains(&"Holder::DEAD_CONST"), "DEAD_CONST has no references, got: {names:?}");
        assert!(!names.contains(&"Holder::LIVE_CONST"), "LIVE_CONST is referenced by x, got: {names:?}");
    }

    #[test]
    fn detect_dead_constants_excludes_infrastructure() {
        let s = server_with_source(
            "
            class DeadClass; end
            class LiveClass; end
            x = LiveClass.new
            ",
        );
        let res = detect_dead_constants!(s, kind: None, file_path: None, limit: None, offset: None);
        let names = result_names(&res, "results");

        assert!(names.contains(&"DeadClass"), "DeadClass should appear, got: {names:?}");
        assert!(
            names.iter().all(|n| !is_infrastructure_declaration(n)),
            "Infrastructure declarations should be excluded, got: {names:?}"
        );
    }

    #[test]
    fn detect_dead_constants_excludes_methods_and_variables() {
        let s = server_with_source(
            "
            class Foo
              def unused_method; end
              LIVE_CONST = 1
              DEAD_CONST = 2
            end
            x = Foo::LIVE_CONST
            ",
        );
        let res = detect_dead_constants!(s, kind: None, file_path: None, limit: None, offset: None);
        let names = result_names(&res, "results");

        assert!(names.contains(&"Foo::DEAD_CONST"), "DEAD_CONST should appear, got: {names:?}");
        assert!(!names.contains(&"Foo::LIVE_CONST"), "LIVE_CONST is referenced, got: {names:?}");
        assert!(
            !names.iter().any(|n| n.contains("unused_method")),
            "Methods should never appear, got: {names:?}"
        );
    }

    #[test]
    fn detect_dead_constants_kind_filter() {
        let s = server_with_source(
            "
            class LiveClass; end
            class DeadClass; end
            module DeadModule; end
            x = LiveClass.new
            ",
        );

        let res = detect_dead_constants!(s, kind: Some("Class".into()), file_path: None, limit: None, offset: None);
        let names = result_names(&res, "results");
        assert!(names.contains(&"DeadClass"), "DeadClass should appear with kind=Class, got: {names:?}");
        assert!(
            !names.contains(&"DeadModule"),
            "Module should be filtered out when kind=Class, got: {names:?}"
        );

        // Case-insensitive
        let res = detect_dead_constants!(s, kind: Some("module".into()), file_path: None, limit: None, offset: None);
        let names = result_names(&res, "results");
        assert!(names.contains(&"DeadModule"), "DeadModule should appear with kind=module, got: {names:?}");
        assert!(!names.contains(&"DeadClass"), "Class should be filtered out when kind=module, got: {names:?}");
    }

    #[test]
    fn detect_dead_constants_pagination() {
        let s = server_with_source(
            "
            class LiveClass; end
            class DeadA; end
            class DeadB; end
            class DeadC; end
            x = LiveClass.new
            ",
        );

        let full = detect_dead_constants!(s, kind: Some("Class".into()), file_path: None, limit: None, offset: None);
        let total = full["total"].as_u64().unwrap();
        assert!(total >= 3, "Expected at least 3 dead classes, got {total}");
        let all_names = result_names(&full, "results");
        assert!(!all_names.contains(&"LiveClass"), "LiveClass should not appear, got: {all_names:?}");

        let page1 = detect_dead_constants!(s, kind: Some("Class".into()), file_path: None, limit: Some(1), offset: Some(0));
        assert_eq!(array!(page1, "results").len(), 1);
        assert_json_int!(page1, "total", total);

        let page2 = detect_dead_constants!(s, kind: Some("Class".into()), file_path: None, limit: Some(1), offset: Some(1));
        let name1 = array!(page1, "results")[0]["name"].as_str().unwrap();
        let name2 = array!(page2, "results")[0]["name"].as_str().unwrap();
        assert_ne!(name1, name2, "Pages should return different items");
    }

    #[test]
    fn detect_dead_constants_file_path_filter() {
        let models = test_uri("app/models/user.rb");
        let services = test_uri("app/services/notifier.rb");
        let s = server_with_sources(&[
            (&models, "class DeadUser; end\nclass LiveUser; end"),
            (
                &services,
                "class DeadNotifier; end\nx = LiveUser",
            ),
        ]);

        let res = detect_dead_constants!(s, kind: None, file_path: Some("app/models".into()), limit: None, offset: None);
        let names = result_names(&res, "results");
        assert!(names.contains(&"DeadUser"), "DeadUser is in app/models and dead, got: {names:?}");
        assert!(!names.contains(&"LiveUser"), "LiveUser is referenced from services, got: {names:?}");
        assert!(
            !names.contains(&"DeadNotifier"),
            "DeadNotifier is in app/services, not app/models, got: {names:?}"
        );
    }

    #[test]
    fn detect_dead_constants_all_referenced() {
        let s = server_with_source(
            "
            class Animal; end
            class Dog < Animal; end
            Dog.new
            ",
        );
        let res = detect_dead_constants!(s, kind: Some("Class".into()), file_path: None, limit: None, offset: None);
        let names = result_names(&res, "results");

        assert!(!names.contains(&"Animal"), "Animal is referenced by Dog, got: {names:?}");
        assert!(!names.contains(&"Dog"), "Dog is referenced by Dog.new, got: {names:?}");
    }

    #[test]
    fn detect_dead_constants_includes_owner() {
        let s = server_with_source(
            "
            class Outer
              class LiveInner; end
              class DeadInner; end
            end
            x = Outer::LiveInner.new
            ",
        );
        let res = detect_dead_constants!(s, kind: None, file_path: None, limit: None, offset: None);
        let names = result_names(&res, "results");

        assert!(names.contains(&"Outer::DeadInner"), "DeadInner has no references, got: {names:?}");
        assert!(!names.contains(&"Outer::LiveInner"), "LiveInner is referenced by x, got: {names:?}");

        let entry = array!(res, "results")
            .iter()
            .find(|e| e["name"].as_str() == Some("Outer::DeadInner"))
            .unwrap();
        assert_eq!(entry["owner"].as_str().unwrap(), "Outer");
    }

    // -- get_file_declarations --

    #[test]
    fn get_file_declarations_success() {
        let s = server_with_source(
            "
            class Animal; end
            class Dog < Animal; end
            module Greetable; end
            ",
        );
        let res = get_file_declarations(&s, "test.rb");

        assert_includes!(res, "declarations", "Animal");
        assert_includes!(res, "declarations", "Dog");
        assert_includes!(res, "declarations", "Greetable");
        assert_eq!(array!(res, "declarations")[0]["name"], "Animal");
        assert_eq!(array!(res, "declarations")[0]["kind"], "Class");
        assert_json_int!(array!(res, "declarations")[0], "line", 1);
    }

    #[test]
    fn get_file_declarations_multiple_files() {
        let models = test_uri("models.rb");
        let services = test_uri("services.rb");
        let s = server_with_sources(&[(&models, "class Animal; end"), (&services, "class Kennel; end")]);
        let res = get_file_declarations(&s, "services.rb");
        assert_includes!(res, "declarations", "Kennel");
    }

    #[test]
    fn get_file_declarations_not_found() {
        let s = server_with_source("class Dog; end");
        assert_error(
            &s.get_file_declarations(Parameters(GetFileDeclarationsParams {
                file_path: "nonexistent.rb".into(),
            })),
            "not_found",
        );
    }

    // -- codebase_stats --

    #[test]
    fn codebase_stats_returns_counts() {
        let a = test_uri("a.rb");
        let b = test_uri("b.rb");
        let s = server_with_sources(&[(&a, "class Animal; end"), (&b, "module Greetable; end")]);
        let res = parse(&s.codebase_stats());

        assert_eq!(res["files"], 2);
        assert_json_int!(res, "declarations", 5);
        assert_json_int!(res, "definitions", 2);

        let breakdown = &res["breakdown_by_kind"];
        assert_json_int!(breakdown, "Class", 4);
        assert_json_int!(breakdown, "Module", 1);
    }

    // -- error states --

    #[test]
    fn returns_indexing_error_when_graph_not_ready() {
        let server = RubydexServer::new("/test".to_string());
        // graph is None (still indexing)
        assert_error(&server.codebase_stats(), "indexing");
    }

    #[test]
    fn returns_indexing_failed_error() {
        let server = RubydexServer::new("/test".to_string());
        {
            let mut state = server.state.write().unwrap();
            state.error = Some("something went wrong".into());
        }
        assert_error(&server.codebase_stats(), "indexing_failed");
    }
}
