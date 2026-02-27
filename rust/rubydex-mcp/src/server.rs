use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use crate::tools::{
    FindConstantReferencesParams, GetDeclarationParams, GetDescendantsParams, GetFileDeclarationsParams,
    SearchDeclarationsParams,
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
    declaration::{Ancestor, Ancestors},
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

/// Formats an ancestor chain into a JSON array of `{"name": ..., "kind": ...}` objects.
fn format_ancestors(graph: &Graph, ancestors: &Ancestors) -> Vec<serde_json::Value> {
    ancestors
        .iter()
        .filter_map(|ancestor| match ancestor {
            Ancestor::Complete(id) => {
                let ancestor_decl = graph.declarations().get(id)?;
                Some(serde_json::json!({
                    "name": ancestor_decl.name(),
                    "kind": ancestor_decl.kind().as_api_str(),
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

#[tool_router]
impl RubydexServer {
    #[tool(
        description = "Search for Ruby classes, modules, methods, or constants by name. Use this INSTEAD OF Grep when you know part of a Ruby identifier name and want to find its definition. Returns fully qualified names, kinds, and file locations. Use the `kind` filter (\"Class\", \"Module\", \"Method\", \"Constant\") to narrow results."
    )]
    fn search_declarations(&self, Parameters(params): Parameters<SearchDeclarationsParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();
        let ids = rubydex::query::declaration_search(graph, &params.query);

        let limit = params.limit.unwrap_or(25).min(100);
        let kind_filter = params.kind.as_deref();

        let mut results: Vec<serde_json::Value> = Vec::new();
        for id in ids {
            if results.len() >= limit {
                break;
            }

            let Some(decl) = graph.declarations().get(&id) else {
                continue;
            };

            if let Some(kind) = kind_filter
                && !decl.kind().as_api_str().eq_ignore_ascii_case(kind)
            {
                continue;
            }

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

            results.push(serde_json::json!({
                "name": decl.name(),
                "kind": decl.kind().as_api_str(),
                "locations": locations,
            }));
        }

        serde_json::to_string(&results).unwrap_or_else(|_| "[]".to_string())
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
                            "kind": member_decl.kind().as_api_str(),
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
            "kind": decl.kind().as_api_str(),
            "definitions": definitions,
            "ancestors": ancestors,
            "members": members,
        });

        serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string())
    }

    #[tool(
        description = "Find all classes and modules that inherit from or include a given class/module. Returns known descendants, including transitive relationships, for impact analysis before modifying a base class or module."
    )]
    fn get_descendants(&self, Parameters(params): Parameters<GetDescendantsParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();
        let (_, decl) = lookup_declaration!(graph, &params.name);
        let namespace = require_namespace!(decl, &params.name, "get_descendants");

        let descendants: Vec<serde_json::Value> = namespace
            .descendants()
            .iter()
            .filter_map(|desc_id| {
                let desc_decl = graph.declarations().get(desc_id)?;
                Some(serde_json::json!({
                    "name": desc_decl.name(),
                    "kind": desc_decl.kind().as_api_str(),
                }))
            })
            .collect();

        let result = serde_json::json!({
            "name": decl.name(),
            "descendants": descendants,
        });

        serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string())
    }

    #[tool(
        description = "Find all resolved references to a Ruby class, module, or constant across the codebase. Returns file paths, line numbers, and columns for each usage."
    )]
    fn find_constant_references(&self, Parameters(params): Parameters<FindConstantReferencesParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();
        let (_, decl) = lookup_declaration!(graph, &params.name);

        let limit = params.limit.unwrap_or(50).min(200);
        let mut references: Vec<serde_json::Value> = Vec::with_capacity(limit.min(decl.references().len()));
        let mut truncated = false;

        for ref_id in decl.references() {
            if references.len() >= limit {
                truncated = true;
                break;
            }

            let Some(const_ref) = graph.constant_references().get(ref_id) else {
                continue;
            };

            let Some(doc) = graph.documents().get(&const_ref.uri_id()) else {
                continue;
            };
            let loc = const_ref.offset().to_location(doc).to_presentation();
            references.push(serde_json::json!({
                "path": format_path(doc.uri(), &self.root_path),
                "line": loc.start_line(),
                "column": loc.start_col(),
            }));
        }

        let mut result = serde_json::json!({
            "name": params.name,
            "references": references,
        });
        if truncated {
            result["truncated"] = serde_json::Value::Bool(true);
        }

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
                .map(|decl| (decl.name().to_string(), decl.kind().as_api_str()));

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
            *breakdown.entry(decl.kind().as_api_str()).or_default() += 1;
        }

        let breakdown_json: serde_json::Value = breakdown
            .iter()
            .map(|(k, v)| ((*k).to_string(), serde_json::json!(v)))
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
- Want general statistics? -> codebase_stats (size and composition)

Typical workflow: search_declarations -> get_declaration -> find_constant_references.

Fully qualified name format: "Foo::Bar" for classes/modules/constants, "Foo::Bar#method_name" for instance methods.

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
