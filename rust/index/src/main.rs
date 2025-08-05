use std::{
    env, fs,
    path::PathBuf,
    sync::{Arc, Mutex},
};

use index::{
    indexing::{Document, index_in_parallel},
    model::index::Index,
};

fn main() {
    let dir = env::args()
        .nth(1)
        .expect("Please provide a directory path as an argument");

    let documents = {
        let mut uris: Vec<String> = Vec::new();
        collect_files_recursive(&PathBuf::from(dir), &mut uris);
        uris.into_iter()
            .map(|path| Document::new(path, None))
            .collect::<Vec<_>>()
    };

    let file_queue = Arc::new(Mutex::new(documents));

    let index = Arc::new(Mutex::new(Index::new()));
    index_in_parallel(&index, &file_queue);
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
                } else if path.is_file() && path.extension().filter(|ext| *ext == "rb").is_some() {
                    if let Ok(absolute_path) = path.canonicalize() {
                        uris.push(format!("file://{}", absolute_path.to_string_lossy()));
                    }
                }
            }
        }
        Err(e) => eprintln!("Error reading directory '{}': {}", directory.display(), e),
    }
}
