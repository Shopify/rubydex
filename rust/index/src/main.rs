use std::{
    env,
    error::Error,
    fs,
    path::PathBuf,
    sync::{Arc, Mutex},
};

use index::{
    indexing::{Document, index_in_parallel},
    model::index::Index,
};

fn main() -> Result<(), Box<dyn Error>> {
    let dir = env::args()
        .nth(1)
        .expect("Please provide a directory path as an argument");

    let documents = {
        let mut uris: Vec<String> = Vec::new();
        collect_files_recursive(&PathBuf::from(dir), &mut uris);
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

    let index = Arc::new(Mutex::new(Index::new()));
    index_in_parallel(&index, &documents)?;
    Ok(())
}

fn collect_files_recursive(directory: &PathBuf, uris: &mut Vec<String>) {
    match fs::read_dir(directory) {
        Ok(entries) => {
            for entry in entries.flatten() {
                let path = entry.path();
                let excluded_dirs = ["tmp", "test", "vendor", ".vscode", "vscode", "fixtures"];

                if path.is_dir()
                    && path
                        .file_name()
                        .is_some_and(|name| !excluded_dirs.contains(&name.to_str().unwrap()))
                {
                    collect_files_recursive(&path, uris);
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
