use std::process::Command;

use clap::Parser;
use rubydex::{
    diff::{self, GraphDiff},
    indexing, listing,
    model::graph::Graph,
    resolution::Resolver,
};

#[derive(Parser, Debug)]
#[command(name = "diff", about = "Diff two git refs to test graph equality")]
struct Args {
    #[arg(help = "Path to git repository")]
    path: String,

    #[arg(help = "First git ref (e.g., main, HEAD~1, abc123)")]
    ref_a: String,

    #[arg(help = "Second git ref")]
    ref_b: String,
}

fn checkout(path: &str, git_ref: &str) -> Result<(), String> {
    let output = Command::new("git")
        .args(["checkout", git_ref])
        .current_dir(path)
        .output()
        .map_err(|e| format!("Failed to run git checkout: {e}"))?;

    if !output.status.success() {
        return Err(format!(
            "git checkout {} failed: {}",
            git_ref,
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(())
}

fn get_current_ref(path: &str) -> Result<String, String> {
    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(path)
        .output()
        .map_err(|e| format!("Failed to run git rev-parse: {e}"))?;

    if !output.status.success() {
        return Err("git rev-parse HEAD failed".to_string());
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn build_graph(path: &str) -> Graph {
    let (file_paths, _) = listing::collect_file_paths(vec![path.to_string()]);

    let mut graph = Graph::new();
    indexing::index_files(&mut graph, file_paths);

    let mut resolver = Resolver::new(&mut graph);
    resolver.resolve_all();

    graph
}

fn print_diff(diff: &GraphDiff, graph_a: &Graph, graph_b: &Graph) {
    if !diff.added_declarations.is_empty() {
        println!("\nAdded declarations ({}):", diff.added_declarations.len());
        for id in &diff.added_declarations {
            if let Some(decl) = graph_b.declarations().get(id) {
                println!("  + {}", decl.name());
            }
        }
    }

    if !diff.removed_declarations.is_empty() {
        println!("\nRemoved declarations ({}):", diff.removed_declarations.len());
        for id in &diff.removed_declarations {
            if let Some(decl) = graph_a.declarations().get(id) {
                println!("  - {}", decl.name());
            }
        }
    }

    if !diff.changed_declarations.is_empty() {
        println!("\nChanged declarations ({}):", diff.changed_declarations.len());
        for id in &diff.changed_declarations {
            if let Some(decl) = graph_a.declarations().get(id) {
                println!("  ~ {}", decl.name());
                print_declaration_diff(graph_a, graph_b, *id);
            }
        }
    }

    if !diff.added_definitions.is_empty() {
        println!("\nAdded definitions: {}", diff.added_definitions.len());
    }
    if !diff.removed_definitions.is_empty() {
        println!("Removed definitions: {}", diff.removed_definitions.len());
    }

    if !diff.added_references.is_empty() {
        println!("\nAdded references: {}", diff.added_references.len());
    }
    if !diff.removed_references.is_empty() {
        println!("Removed references: {}", diff.removed_references.len());
    }

    if !diff.added_names.is_empty() {
        println!("\nAdded names: {}", diff.added_names.len());
    }
    if !diff.removed_names.is_empty() {
        println!("Removed names: {}", diff.removed_names.len());
    }
    if !diff.changed_names.is_empty() {
        println!("Changed names: {}", diff.changed_names.len());
    }
}

fn print_declaration_diff(graph_a: &Graph, graph_b: &Graph, id: rubydex::model::ids::DeclarationId) {
    let (Some(decl_a), Some(decl_b)) = (graph_a.declarations().get(&id), graph_b.declarations().get(&id)) else {
        return;
    };

    let (Some(ns_a), Some(ns_b)) = (decl_a.as_namespace(), decl_b.as_namespace()) else {
        return;
    };

    if ns_a.members() != ns_b.members() {
        for (str_id, decl_id) in ns_a.members() {
            if !ns_b.members().contains_key(str_id)
                && let Some(decl) = graph_a.declarations().get(decl_id)
            {
                println!("      - {}", decl.name());
            }
        }
        for (str_id, decl_id) in ns_b.members() {
            if !ns_a.members().contains_key(str_id)
                && let Some(decl) = graph_b.declarations().get(decl_id)
            {
                println!("      + {}", decl.name());
            }
        }
    }

    let anc_a = ns_a.ancestors();
    let anc_b = ns_b.ancestors();
    let ancestors_a: Vec<_> = anc_a.iter().collect();
    let ancestors_b: Vec<_> = anc_b.iter().collect();
    if ancestors_a != ancestors_b {
        println!("      ancestors differ");
    }

    if ns_a.descendants() != ns_b.descendants() {
        println!("      descendants differ");
    }

    if ns_a.singleton_class() != ns_b.singleton_class() {
        println!("      singleton_class differs");
    }
}

fn main() -> Result<(), String> {
    let args = Args::parse();

    let original_ref = get_current_ref(&args.path)?;

    println!("Checking out {}...", args.ref_a);
    checkout(&args.path, &args.ref_a)?;
    println!("Building graph for {}...", args.ref_a);
    let graph_a = build_graph(&args.path);
    println!(
        "  {} declarations, {} definitions",
        graph_a.declarations().len(),
        graph_a.definitions().len()
    );

    println!("Checking out {}...", args.ref_b);
    checkout(&args.path, &args.ref_b)?;
    println!("Building graph for {}...", args.ref_b);
    let graph_b = build_graph(&args.path);
    println!(
        "  {} declarations, {} definitions",
        graph_b.declarations().len(),
        graph_b.definitions().len()
    );

    println!("Restoring {original_ref}...");
    checkout(&args.path, &original_ref)?;

    println!("\nComparing graphs...");
    match diff::diff(&graph_a, &graph_b) {
        Some(diff) => {
            println!("Graphs differ!");
            print_diff(&diff, &graph_a, &graph_b);
        }
        None => {
            println!("Graphs are identical!");
        }
    }

    Ok(())
}
