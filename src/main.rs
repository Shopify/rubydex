use std::process;
use std::path::Path;
use glob::glob;
use std::collections::HashMap;
use ruby_prism::Visit;
use clap::{Parser, ValueEnum};
use pool::{Pool, PoolId};
use tables::NameId;

mod ast_enum;
mod ast_data;
mod location;
mod pool;
mod tables;
mod locations;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Mode {
    Data,
    Enum,
    Locations,
}

#[derive(Parser)]
#[command(name = "index")]
#[command(about = "A Ruby file indexer")]
struct Args {
    #[arg(required = true)]
    paths: Vec<String>,

    #[arg(long, value_enum, default_value_t = Mode::Data)]
    mode: Mode,

    #[arg(long, default_value_t = false)]
    print: bool,

    #[arg(long, default_value_t = false)]
    simulate_cache: bool,
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
                            if !files.contains(&path_str) && Path::new(&path_str).is_file() {
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

fn process_files_data(files: &[String], print_symbols: bool, simulate_cache: bool) {
    let mut tables = tables::GlobalTables::new();
    let mut symbols_table: HashMap<PoolId<NameId>, Vec<ast_data::symbol::Symbol>> = HashMap::new();

    for file in files {
        let source = match std::fs::read_to_string(file) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("Error reading file {}: {}", file, err);
                return;
            }
        };

        let result = ruby_prism::parse(source.as_ref());
        let mut visitor = ast_data::visitor::Visitor::new(&mut tables, file, &mut symbols_table, !simulate_cache);
        visitor.visit(&result.node());
    }

    println!("  Found {} symbols.", symbols_table.values().flatten().count());

    if print_symbols {
        for (_, symbols) in symbols_table.iter() {
            for symbol in symbols {
                println!("{}", symbol.to_string(&tables));
            }
        }
    }
}

fn process_files_enum(files: &[String], print_symbols: bool) {
    let mut tables = tables::GlobalTables::new();
    let mut symbols_table: HashMap<PoolId<NameId>, Vec<ast_enum::symbol::Symbol>> = HashMap::new();

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

    println!("  Found {} symbols.", symbols_table.values().flatten().count());

    if print_symbols {
        for (_, symbols) in symbols_table.iter() {
            for symbol in symbols {
                println!("{}", symbol.to_string(&tables));
            }
        }
    }
}

fn locations(files: &[String], print_locations: bool) {
    println!("  Struct size: {:?}", std::mem::size_of::<locations::Location>());

    let mut file_pool = Pool::<locations::FileId, String>::new();

    let mut locations = Vec::new();
    for file in files {
        let file_id = file_pool.add(file.clone());

        let location = locations::Location {
            file_id,
            start_offset: 0,
            end_offset: 0,
        };

        locations.push(location);

        let source = match std::fs::read_to_string(file) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("Error reading file {}: {}", file, err);
                return;
            }
        };

        let result = ruby_prism::parse(source.as_ref());
        let mut visitor = locations::Visitor::new(&mut locations, file_id);
        visitor.visit(&result.node());
    }

    println!("  Found {} locations.", locations.len());

    if print_locations {
        for location in locations {
            location.show();
        }
    }
}

fn main() {
    let args = Args::parse();

    let files = collect_files(&args.paths);
    println!("  Found {} files.", files.len());

    match args.mode {
        Mode::Data => process_files_data(&files, args.print, args.simulate_cache),
        Mode::Enum => process_files_enum(&files, args.print),
        Mode::Locations => locations(&files, args.print),
    }

    process::exit(0);
}