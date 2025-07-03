use std::process;
use std::path::Path;
use glob::glob;
use std::collections::HashMap;
use ast_enum::symbol::{Symbol};
use ast_enum::visitor::{Visitor};
use ruby_prism::Visit;
use clap::{Parser, ValueEnum};

mod ast_enum;
mod ast_data;
mod ast_base;
mod location;
mod pool;
mod tables;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Mode {
    Base,
    Data,
    Enum,
}

#[derive(Parser)]
#[command(name = "index")]
#[command(about = "A Ruby file indexer")]
struct Args {
    #[arg(required = true)]
    paths: Vec<String>,

    #[arg(long, value_enum, default_value_t = Mode::Data)]
    mode: Mode,
}

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

fn process_files_base(files: &[String]) {
    let mut tables = tables::GlobalTables::new();
    let mut symbols_table: HashMap<String, ast_base::symbol::Symbol> = HashMap::new();

    for file in files {
        let source = match std::fs::read_to_string(file) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("Error reading file {}: {}", file, err);
                return;
            }
        };

        let result = ruby_prism::parse(source.as_ref());
        let mut visitor = ast_base::visitor::Visitor::new(&mut tables, file, &mut symbols_table);
        visitor.visit(&result.node());
    }

    println!("  Found {} symbols.", symbols_table.len());

    for (_, symbol) in symbols_table.iter() {
        println!("{}", symbol.to_string(&tables));
    }
}

fn process_files_data(files: &[String]) {
    let mut tables = tables::GlobalTables::new();
    let mut symbols_table: HashMap<String, ast_data::symbol::Symbol> = HashMap::new();

    for file in files {
        let source = match std::fs::read_to_string(file) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("Error reading file {}: {}", file, err);
                return;
            }
        };

        let result = ruby_prism::parse(source.as_ref());
        let mut visitor = ast_data::visitor::Visitor::new(&mut tables, file, &mut symbols_table);
        visitor.visit(&result.node());
    }

    println!("  Found {} symbols.", symbols_table.len());

    // for (_, symbol) in symbols_table.iter() {
    //     println!("{}", symbol);
    // }
}

fn process_files_enum(files: &[String]) {
    let mut tables = tables::GlobalTables::new();
    let mut symbols_table: HashMap<String, ast_enum::symbol::Symbol> = HashMap::new();

    for file in files {
        let source = match std::fs::read_to_string(file) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("Error reading file {}: {}", file, err);
                return;
            }
        };

        let result = ruby_prism::parse(source.as_ref());
        let mut visitor = ast_enum::visitor::Visitor::new(&mut tables, file, &mut symbols_table);
        visitor.visit(&result.node());
    }

    println!("  Found {} symbols.", symbols_table.len());

    // for (_, symbol) in symbols_table.iter() {
    //     println!("{}", symbol);
    // }
}

fn main() {
    let args = Args::parse();

    let files = collect_files(&args.paths);
    println!("  Found {} files.", files.len());

    match args.mode {
        Mode::Base => process_files_base(&files),
        Mode::Data => process_files_data(&files),
        Mode::Enum => process_files_enum(&files),
    }

    process::exit(0);
}