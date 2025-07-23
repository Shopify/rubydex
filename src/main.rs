use glob::glob;
use ruby_prism::Visit;
use std::env;
use std::path::Path;
use std::process;

use model::builder::Builder;
use model::symbol_definitions::SymbolDefinition;
use pools::name_pool::NamePool;
use pools::uri_pool::UriPool;

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

fn process_files(paths: &[String], uri_pool: &mut UriPool, name_pool: &mut NamePool) -> Vec<SymbolDefinition> {
    let mut symbol_definitions: Vec<SymbolDefinition> = Vec::new();

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
        Builder::new(uri_id, name_pool, &mut symbol_definitions).visit(&result.node());
    }

    symbol_definitions
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let files = collect_files(&args);
    println!("  Found {} files.", files.len());

    let mut uri_pool = UriPool::new();
    let mut name_pool = NamePool::new();

    let symbol_definitions = process_files(&files, &mut uri_pool, &mut name_pool);
    println!("  Found {} symbols.", symbol_definitions.len());

    process::exit(0);
}
