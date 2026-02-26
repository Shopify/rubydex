use schemars::JsonSchema;

#[derive(Debug, serde::Deserialize, JsonSchema)]
pub struct SearchDeclarationsParams {
    #[schemars(description = "Search query to fuzzy match against declaration names")]
    pub query: String,
    #[schemars(description = "Filter by declaration kind: Class, Module, Method, Constant, etc.")]
    pub kind: Option<String>,
    #[schemars(description = "Maximum number of results to return (default 25, max 100)")]
    pub limit: Option<usize>,
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
}

#[derive(Debug, serde::Deserialize, JsonSchema)]
pub struct FindConstantReferencesParams {
    #[schemars(description = "Fully qualified name of the class, module, or constant to find references for")]
    pub name: String,
    #[schemars(description = "Maximum number of references to return (default 50, max 200)")]
    pub limit: Option<usize>,
}

#[derive(Debug, serde::Deserialize, JsonSchema)]
pub struct GetFileDeclarationsParams {
    #[schemars(description = "File path (relative or absolute) to list declarations for")]
    pub file_path: String,
}
