use std::process;
use std::path::Path;
use glob::glob;
use std::collections::HashMap;
use ast::symbol::{Symbol};
use ast::visitor::{Visitor};
use ruby_prism::Visit;

mod ast;

fn glob_files(dir_path: &str) -> Vec<String> {
    let mut files = Vec::new();

    // Different patterns you can use:
    let patterns = vec![
        format!("{}/**/*.rb", dir_path),     // All .rb files recursively
    ];

    for pattern in patterns {
        match glob(&pattern) {
            Ok(paths) => {
                for entry in paths {
                    match entry {
                        Ok(file_path) => {
                            let path_str = file_path.to_string_lossy().to_string();
                            if !files.contains(&path_str) {
                                files.push(path_str);
                            }
                        }
                        Err(e) => eprintln!("Error reading glob entry: {}", e),
                    }
                }
            }
            Err(e) => eprintln!("Failed to read glob pattern {}: {}", pattern, e),
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

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <path> [path] ...", args[0]);
        process::exit(1);
    }

    let files = collect_files(&args[1..]);
    println!("  Found {} files.", files.len());

    let mut symbols_table: HashMap<String, Symbol> = HashMap::new();

    for file in &files {
        let source = match std::fs::read_to_string(file) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("Error reading file {}: {}", file, err);
                return;
            }
        };

        let result = ruby_prism::parse(source.as_ref());
        let mut visitor = Visitor::new(file, &mut symbols_table);
        visitor.visit(&result.node());
    }

    println!("  Found {} symbols.", symbols_table.len());

    for (_, symbol) in symbols_table {
        // println!("  {:?} {} {}", symbol.kind, symbol.name, symbol.location);
    }

    process::exit(0);
}