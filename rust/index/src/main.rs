use std::collections::HashSet;
use std::{error::Error, fs, path::PathBuf, sync::Arc};

use clap::Parser;

use index::{
    indexing::{Document, index_in_parallel},
    model::graph::Graph,
};

#[derive(Parser, Debug)]
#[command(name = "index_cli", about = "A Ruby code indexer", version)]
struct Args {
    #[arg(value_name = "DIR", default_value = ".")]
    dir: String,

    #[arg(long = "exclude", value_name = "NAME", num_args = 0.., default_values = [
        "tmp",
        "test",
        "vendor",
        ".vscode",
        "vscode",
        "fixtures",
    ])]
    exclude: Vec<String>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let excluded_dirs_set: HashSet<&str> = args.exclude.iter().map(String::as_str).collect();

    let documents = {
        let mut uris: Vec<String> = Vec::new();
        collect_files_recursive(&PathBuf::from(args.dir), &excluded_dirs_set, &mut uris);
        uris.into_iter()
            .filter_map(|uri| match Document::new(&uri, None) {
                Ok(document) => Some(document),
                Err(e) => {
                    eprintln!("Invalid URI for document '{uri}': {e}");
                    None
                }
            })
            .collect::<Vec<_>>()
    };

    let graph = Arc::new(Graph::new());
    index_in_parallel(&graph, &documents)?;
    Ok(())
}

fn collect_files_recursive(directory: &PathBuf, excluded_dirs: &HashSet<&str>, uris: &mut Vec<String>) {
    match fs::read_dir(directory) {
        Ok(entries) => {
            for entry in entries.flatten() {
                let path = entry.path();

                if path.is_dir()
                    && path
                        .file_name()
                        .is_some_and(|name| !excluded_dirs.contains(&name.to_str().unwrap()))
                {
                    collect_files_recursive(&path, excluded_dirs, uris);
                } else if path.is_file()
                    && path.extension().filter(|ext| *ext == "rb").is_some()
                    && let Ok(absolute_path) = path.canonicalize()
                {
                    uris.push(format!("file://{}", absolute_path.to_string_lossy()));
                }
            }
        }
        Err(e) => eprintln!("Error reading directory '{}': {}", directory.display(), e),
    }
}
