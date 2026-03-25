use schemars::JsonSchema;

#[derive(Debug, serde::Deserialize, JsonSchema)]
pub struct SearchDeclarationsParams {
    #[schemars(description = "Search query to match against declaration names")]
    pub query: String,
    #[schemars(description = "Filter by declaration kind: Class, Module, Method, Constant, etc.")]
    pub kind: Option<String>,
    #[schemars(
        description = "Matching mode: \"fuzzy\" (default) for LSP-style workspace symbol search, or \"exact\" for precise substring matching"
    )]
    pub match_mode: Option<String>,
    #[schemars(description = "Maximum number of results to return (default 50, max 100)")]
    pub limit: Option<usize>,
    #[schemars(description = "Number of results to skip for pagination (default 0)")]
    pub offset: Option<usize>,
}

#[derive(Debug, serde::Deserialize, JsonSchema)]
pub struct GetDeclarationParams {
    #[schemars(description = "Fully qualified name of the declaration (e.g. 'Foo::Bar', 'Foo::Bar#baz')")]
    pub name: String,
}

#[derive(Debug, serde::Deserialize, JsonSchema)]
pub struct GetDescendantsParams {
    #[schemars(description = "Fully qualified name of the class or module")]
    pub name: String,
    #[schemars(description = "Maximum number of descendants to return (default 50, max 500)")]
    pub limit: Option<usize>,
    #[schemars(description = "Number of descendants to skip for pagination (default 0)")]
    pub offset: Option<usize>,
}

#[derive(Debug, serde::Deserialize, JsonSchema)]
pub struct FindConstantReferencesParams {
    #[schemars(description = "Fully qualified name of the class, module, or constant to find references for")]
    pub name: String,
    #[schemars(description = "Maximum number of references to return (default 50, max 200)")]
    pub limit: Option<usize>,
    #[schemars(description = "Number of references to skip for pagination (default 0)")]
    pub offset: Option<usize>,
}

#[derive(Debug, serde::Deserialize, JsonSchema)]
pub struct GetFileDeclarationsParams {
    #[schemars(description = "File path (relative or absolute) to list declarations for")]
    pub file_path: String,
}
