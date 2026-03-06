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
use rubydex::model::graph::Graph;

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


#[tool_router]
impl RubydexServer {
    #[tool(
        description = "Search for Ruby classes, modules, methods, or constants by name. Use this INSTEAD OF Grep when you know part of a Ruby identifier name and want to find its definition. Returns fully qualified names, kinds, and file locations. Use the `kind` filter (\"Class\", \"Module\", \"Method\", \"Constant\") to narrow results. Results are paginated: the response includes `total` (the full count of matches). If `total` exceeds the number of returned results, use `offset` to fetch subsequent pages."
    )]
    fn search_declarations(&self, Parameters(params): Parameters<SearchDeclarationsParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();
        let limit = params.limit.filter(|&l| l > 0).unwrap_or(50).min(100);
        let offset = params.offset.unwrap_or(0);
        let result = crate::queries::query_search_declarations(
            graph,
            &self.root_path,
            &params.query,
            params.kind.as_deref(),
            limit,
            offset,
        );
        serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string())
    }

    #[tool(
        description = "Get complete information about a Ruby class, module, method, or constant by its exact fully qualified name. Returns file locations, documentation comments, ancestor chain, and members with locations. FQN format: \"Foo::Bar\" for classes/modules/constants, \"Foo::Bar#method_name\" for instance methods."
    )]
    fn get_declaration(&self, Parameters(params): Parameters<GetDeclarationParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();
        match crate::queries::query_get_declaration(graph, &self.root_path, &params.name) {
            Ok(result) => serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string()),
            Err(e) => e.to_json_string(),
        }
    }

    #[tool(
        description = "Returns all known descendants for the given namespace including itself and all transitive descendants. Can be used to understand how a module/class is used across the codebase. Results are paginated: the response includes `total`. If `total` exceeds the number of returned results, use `offset` to fetch subsequent pages."
    )]
    fn get_descendants(&self, Parameters(params): Parameters<GetDescendantsParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();
        let limit = params.limit.filter(|&l| l > 0).unwrap_or(50).min(500);
        let offset = params.offset.unwrap_or(0);
        match crate::queries::query_get_descendants(graph, &params.name, limit, offset) {
            Ok(result) => serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string()),
            Err(e) => e.to_json_string(),
        }
    }

    #[tool(
        description = "Find all resolved references to a Ruby class, module, or constant across the codebase. Returns file paths, line numbers, and columns for each usage. Results are paginated: the response includes `total`. If `total` exceeds the number of returned results, use `offset` to fetch subsequent pages."
    )]
    fn find_constant_references(&self, Parameters(params): Parameters<FindConstantReferencesParams>) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();
        let limit = params.limit.filter(|&l| l > 0).unwrap_or(50).min(200);
        let offset = params.offset.unwrap_or(0);
        match crate::queries::query_find_constant_references(graph, &self.root_path, &params.name, limit, offset) {
            Ok(result) => serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string()),
            Err(e) => e.to_json_string(),
        }
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
        match crate::queries::query_get_file_declarations(graph, &self.root_path, &canonical_target) {
            Ok(result) => serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string()),
            Err(e) => e.to_json_string(),
        }
    }

    #[tool(
        description = "Get an overview of the indexed Ruby codebase: total file count, declaration counts, and breakdown by kind (classes, modules, methods, constants). Use this to understand codebase size and composition, or to verify that indexing completed successfully."
    )]
    fn codebase_stats(&self) -> String {
        let state = ensure_graph_ready!(self);
        let graph = state.graph.as_ref().unwrap();
        let result = crate::queries::query_codebase_stats(graph);
        serde_json::to_string(&result).unwrap_or_else(|_| "{}".to_string())
    }
}

const SERVER_INSTRUCTIONS: &str = r#"Rubydex provides semantic Ruby code intelligence.

ONLY use these tools for Ruby files (.rb, .rbi, .rbs) — never for Rust, JavaScript, or other languages.

Use these tools INSTEAD OF Grep when working with Ruby code structure.

Decision guide:
- Know a name? -> search_declarations (fuzzy search by name)
- Have an exact fully qualified name? -> get_declaration (full details with docs, ancestors, members)
- Need reverse hierarchy? -> get_descendants (what inherits from this class/module)
- Refactoring a class/module/constant? -> find_constant_references (all precise usages across codebase)
- Exploring a file? -> get_file_declarations (structural overview)
- Want general statistics? -> codebase_stats (size and composition)

Typical workflow: search_declarations -> get_declaration -> find_constant_references.

Fully qualified name format: "Foo::Bar" for classes/modules/constants, "Foo::Bar#method_name" for instance methods.

Pagination: tools that may return a high number of results include `total` for pagination. When `total` exceeds the number of returned items, use `offset` to fetch the next page.

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

    // -- JSON shape tests (one per handler) --

    #[test]
    fn search_declarations_json_shape() {
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
    fn get_declaration_json_shape() {
        let s = server_with_source(
            "
            class Animal; end
            class Dog < Animal
              def speak; end
            end
            ",
        );
        let res = get_declaration(&s, "Dog");

        assert_eq!(res["name"], "Dog");
        assert_eq!(res["kind"], "Class");
        assert!(!array!(res, "definitions").is_empty());
        assert_includes!(res, "ancestors", "Animal");
        assert_includes!(res, "members", "Dog#speak()");

        let member = array!(res, "members")
            .iter()
            .find(|m| m["name"].as_str() == Some("Dog#speak()"))
            .unwrap();
        assert_eq!(member["kind"], "Method");
        assert!(member["location"]["path"].as_str().unwrap().ends_with("test.rb"));
    }

    #[test]
    fn get_descendants_json_shape() {
        let s = server_with_source(
            "
            class Animal; end
            class Dog < Animal; end
            ",
        );
        let res = get_descendants!(s, name: "Animal".into(), limit: None, offset: None);
        assert_eq!(res["name"], "Animal");
        assert_includes!(res, "descendants", "Dog");
        assert!(res["total"].as_u64().unwrap() > 0);
    }

    #[test]
    fn find_constant_references_json_shape() {
        let s = server_with_source(
            "
            class Animal; end
            class Dog < Animal; end
            ",
        );
        let res = find_constant_references!(s, name: "Animal".into(), limit: None, offset: None);

        assert_eq!(res["name"], "Animal");
        assert!(res["total"].as_u64().unwrap() > 0);
        let first_ref = &array!(res, "references")[0];
        assert!(first_ref["path"].as_str().is_some());
        assert!(first_ref["line"].as_u64().is_some());
        assert!(first_ref["column"].as_u64().is_some());
    }

    #[test]
    fn get_file_declarations_json_shape() {
        let s = server_with_source("class Animal; end");
        let res = get_file_declarations(&s, "test.rb");

        assert_includes!(res, "declarations", "Animal");
        assert_eq!(array!(res, "declarations")[0]["kind"], "Class");
        assert!(array!(res, "declarations")[0]["line"].as_u64().is_some());
    }

    #[test]
    fn codebase_stats_json_shape() {
        let a = test_uri("a.rb");
        let b = test_uri("b.rb");
        let s = server_with_sources(&[(&a, "class Animal; end"), (&b, "module Greetable; end")]);
        let res = parse(&s.codebase_stats());

        assert_eq!(res["files"], 2);
        assert!(res["declarations"].as_u64().is_some());
        assert!(res["definitions"].as_u64().is_some());
        assert!(res["breakdown_by_kind"]["Class"].as_u64().is_some());
    }

    // -- error tests --

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

    // -- server state errors --

    #[test]
    fn returns_indexing_error_when_graph_not_ready() {
        let server = RubydexServer::new("/test".to_string());
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
