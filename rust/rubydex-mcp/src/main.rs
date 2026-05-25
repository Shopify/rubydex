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
    #[arg(value_name = "PATH")]
    paths: Vec<String>,
}

fn main() {
    let args = Args::parse();
    let paths = if args.paths.is_empty() {
        vec![".".to_string()]
    } else {
        args.paths
    };

    let paths: Vec<String> = paths.into_iter().map(canonicalize_path).collect();
    let root = paths.first().expect("expected at least one path").clone();

    // Create the server and start indexing in the background.
    let server = server::RubydexServer::new(root.clone());
    server.spawn_indexer(paths);

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

fn canonicalize_path(path: String) -> String {
    match std::fs::canonicalize(&path) {
        Ok(p) => p.into_os_string().into_string().unwrap_or_else(|_| {
            eprintln!("Warning: canonicalized path for '{path}' is not valid UTF-8, using original");
            path
        }),
        Err(e) => {
            eprintln!("Warning: failed to canonicalize '{path}': {e}");
            path
        }
    }
}
