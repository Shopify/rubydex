use clap::Parser;

mod server;
mod tools;

#[derive(Parser, Debug)]
#[command(
    name = "rubydex_mcp",
    about = "Rubydex MCP server for AI-assisted Ruby code intelligence",
    version
)]
struct Args {
    #[arg(value_name = "PATH", default_value = ".")]
    path: String,
}

fn main() {
    let args = Args::parse();

    let root = match std::fs::canonicalize(&args.path) {
        Ok(p) => p
            .into_os_string()
            .into_string()
            .expect("Project path is not valid UTF-8"),
        Err(e) => {
            eprintln!("Warning: failed to canonicalize '{}': {e}", args.path);
            args.path
        }
    };

    // Create the server and start indexing in the background.
    let server = server::RubydexServer::new(root.clone());
    server.spawn_indexer(root);

    // Serve MCP over stdio immediately while indexing runs.
    // We need to do this because Claude Code's default MCP server timeout is 30 seconds,
    // And in big codebases it's possible to exceed that and Claude Code would just consider
    // the server fail to connect.
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .expect("Failed to build tokio runtime");

    if let Err(e) = rt.block_on(server.serve()) {
        eprintln!("MCP server error: {e}");
        std::process::exit(1);
    }
}
