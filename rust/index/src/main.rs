use glob::glob;
use ruby_prism::Visit;
use std::collections::HashMap;
use std::env;
use std::path::Path;
use std::process;

use indexing::ruby_indexer::RubyIndexer;
use model::declaration::Declaration;
use pools::name_pool::{NameId, NamePool};
use pools::uri_pool::UriPool;

mod indexing;
mod model;
mod offset;
mod pools;

fn glob_files(dir_path: &str) -> Vec<String> {
    let mut files = Vec::new();

    let patterns = vec![format!("{}/**/*.rb", dir_path)];

    for pattern in patterns {
        match glob(&pattern) {
            Ok(paths) => {
                for entry in paths {
                    match entry {
                        Ok(file_path) => {
                            let path_str = file_path.to_string_lossy().to_string();
                            if !files.contains(&path_str) && Path::new(&path_str).is_file() {
                                files.push(path_str);
                            }
                        }
                        Err(e) => eprintln!("Error reading glob entry: {e}"),
                    }
                }
            }
            Err(e) => eprintln!("Failed to read glob pattern {pattern}: {e}"),
        }
    }

    files
}

fn collect_files(paths: &[String]) -> Vec<String> {
    let mut files = Vec::new();
    for path in paths {
        if Path::new(path).is_dir() {
            let mut dir_files = glob_files(path);
            files.append(&mut dir_files);
        } else {
            files.push(path.to_string());
        }
    }
    files.sort();
    files.dedup();
    files
}

fn process_files(paths: &[String], uri_pool: &mut UriPool, name_pool: &mut NamePool) -> HashMap<NameId, Declaration> {
    let mut declarations: HashMap<NameId, Declaration> = HashMap::new();

    for path in paths {
        let uri_id = uri_pool.add(format!("file://{path}"));

        let source = match std::fs::read_to_string(path) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("Error reading file {path}: {err}");
                continue;
            }
        };

        let result = ruby_prism::parse(source.as_ref());
        RubyIndexer::new(uri_id, name_pool, &mut declarations).visit(&result.node());
    }

    declarations
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let files = collect_files(&args);
    println!("  Found {} files.", files.len());

    let mut uri_pool = UriPool::new();
    let mut name_pool = NamePool::new();

    let definitions = process_files(&files, &mut uri_pool, &mut name_pool);
    println!("  Found {} definitions.", definitions.len());

    process::exit(0);
}
