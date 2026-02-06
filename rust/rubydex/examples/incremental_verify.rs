use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;

use clap::Parser;
use rubydex::{
    diff::{self, GraphDiff},
    indexing::{self, IndexResult},
    listing,
    model::{
        declaration::Ancestor,
        graph::Graph,
        identity_maps::IdentityHashSet,
        ids::{DeclarationId, NameId},
        name::NameRef,
    },
    resolution::Resolver,
};
use url::Url;

#[derive(Parser, Debug)]
#[command(
    name = "incremental_verify",
    about = "Verify incremental resolution matches full resolution between two git refs"
)]
struct Args {
    #[arg(help = "Path to git repository")]
    path: String,

    #[arg(help = "Base git ref (e.g., main, HEAD~1, abc123)")]
    base_ref: String,

    #[arg(help = "Updated git ref")]
    updated_ref: String,

    #[arg(long, short, help = "Show detailed diff output")]
    verbose: bool,
}

fn git(path: &str, args: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(path)
        .output()
        .map_err(|e| format!("Failed to run git {}: {e}", args.join(" ")))?;

    if !output.status.success() {
        return Err(format!(
            "git {} failed: {}",
            args.join(" "),
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn changed_files(path: &Path, base: &str, updated: &str) -> Result<(Vec<PathBuf>, Vec<PathBuf>), String> {
    let output = git(
        path.to_str().unwrap(),
        &["diff", "--relative", "--name-status", base, updated],
    )?;

    let mut modified = Vec::new();
    let mut deleted = Vec::new();

    for line in output.lines() {
        let mut parts = line.splitn(2, '\t');
        let Some(status) = parts.next() else {
            continue;
        };
        let Some(file) = parts.next() else { continue };

        if !Path::new(file)
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("rb"))
        {
            continue;
        }

        let full_path = path.join(file);
        match status {
            "D" => deleted.push(full_path),
            _ => modified.push(full_path),
        }
    }

    Ok((modified, deleted))
}

fn build_graph(path: &str) -> (Graph, std::time::Duration) {
    let start = Instant::now();
    let (file_paths, _) = listing::collect_file_paths(vec![path.to_string()]);
    let mut graph = Graph::new();
    indexing::index_files(&mut graph, file_paths);
    let mut resolver = Resolver::new(&mut graph);
    resolver.resolve_all();
    (graph, start.elapsed())
}

fn file_uri(path: &Path) -> String {
    Url::from_file_path(path)
        .expect("Failed to build URI from path")
        .to_string()
}

fn name_str(graph: &Graph, name_id: NameId) -> String {
    graph
        .names()
        .get(&name_id)
        .and_then(|name| graph.strings().get(name.str()))
        .map_or_else(|| format!("{name_id}"), |s| s.to_string())
}

fn decl_name(graph: &Graph, id: DeclarationId) -> String {
    graph
        .declarations()
        .get(&id)
        .map_or("?".to_string(), |d| d.name().to_string())
}

fn describe_name(graph: &Graph, name_id: NameId) -> String {
    let Some(name_ref) = graph.names().get(&name_id) else {
        return "missing".to_string();
    };
    let str = name_str(graph, name_id);
    let resolution = match name_ref {
        NameRef::Resolved(resolved) => {
            format!("resolved -> {}", decl_name(graph, *resolved.declaration_id()))
        }
        NameRef::Unresolved(_) => "unresolved".to_string(),
    };
    let deps = name_ref.dependents().len();
    format!("\"{str}\" ({resolution}, {deps} dependents)")
}

fn describe_ancestor(graph: &Graph, ancestor: &Ancestor) -> String {
    match ancestor {
        Ancestor::Complete(id) => {
            let kind = graph.declarations().get(id).map_or("?", |d| d.kind());
            format!("{} [{}]", decl_name(graph, *id), kind)
        }
        Ancestor::Partial(name_id) => {
            let str = name_str(graph, *name_id);
            format!("{str} (partial)")
        }
    }
}

fn print_summary(diff: &GraphDiff) {
    let mut parts = Vec::new();
    let add = diff.added_declarations.len();
    let rem = diff.removed_declarations.len();
    let chg = diff.changed_declarations.len();
    if add > 0 {
        parts.push(format!("+{add} declarations"));
    }
    if rem > 0 {
        parts.push(format!("-{rem} declarations"));
    }
    if chg > 0 {
        parts.push(format!("~{chg} declarations"));
    }

    let add = diff.added_definitions.len();
    let rem = diff.removed_definitions.len();
    if add > 0 {
        parts.push(format!("+{add} definitions"));
    }
    if rem > 0 {
        parts.push(format!("-{rem} definitions"));
    }

    let add = diff.added_references.len();
    let rem = diff.removed_references.len();
    if add > 0 {
        parts.push(format!("+{add} references"));
    }
    if rem > 0 {
        parts.push(format!("-{rem} references"));
    }

    let add = diff.added_names.len();
    let rem = diff.removed_names.len();
    let chg = diff.changed_names.len();
    if add > 0 {
        parts.push(format!("+{add} names"));
    }
    if rem > 0 {
        parts.push(format!("-{rem} names"));
    }
    if chg > 0 {
        parts.push(format!("~{chg} names"));
    }

    println!("  {}", parts.join(", "));
}

#[allow(clippy::too_many_lines)]
fn print_verbose_diff(diff: &GraphDiff, incremental: &Graph, reference: &Graph) {
    if !diff.added_declarations.is_empty() {
        println!(
            "\n  Added declarations (in reference only) ({}):",
            diff.added_declarations.len()
        );
        for id in &diff.added_declarations {
            println!("    + {}", decl_name(reference, *id));
        }
    }

    if !diff.removed_declarations.is_empty() {
        println!(
            "\n  Removed declarations (in incremental only) ({}):",
            diff.removed_declarations.len()
        );
        for id in &diff.removed_declarations {
            println!("    - {}", decl_name(incremental, *id));
        }
    }

    if !diff.changed_declarations.is_empty() {
        let mut extra_in_inc: HashMap<String, usize> = HashMap::new();
        let mut extra_in_ref: HashMap<String, usize> = HashMap::new();
        let mut member_changes = 0;
        let mut descendant_changes = 0;
        let mut singleton_changes = 0;

        for id in &diff.changed_declarations {
            let (Some(decl_a), Some(decl_b)) = (incremental.declarations().get(id), reference.declarations().get(id)) else {
                continue;
            };
            let (Some(ns_a), Some(ns_b)) = (decl_a.as_namespace(), decl_b.as_namespace()) else {
                continue;
            };
            if ns_a.members() != ns_b.members() {
                member_changes += 1;
            }
            if ns_a.descendants() != ns_b.descendants() {
                descendant_changes += 1;
            }
            if ns_a.singleton_class() != ns_b.singleton_class() {
                singleton_changes += 1;
            }

            let anc_a: Vec<_> = ns_a.ancestors().iter().collect();
            let anc_b: Vec<_> = ns_b.ancestors().iter().collect();
            if anc_a != anc_b {
                let set_a: HashSet<_> = anc_a.iter().map(|a| describe_ancestor(incremental, a)).collect();
                let set_b: HashSet<_> = anc_b.iter().map(|a| describe_ancestor(reference, a)).collect();
                for name in set_a.difference(&set_b) {
                    *extra_in_inc.entry(name.clone()).or_default() += 1;
                }
                for name in set_b.difference(&set_a) {
                    *extra_in_ref.entry(name.clone()).or_default() += 1;
                }
            }
        }

        println!("\n  Changed declarations ({}):", diff.changed_declarations.len());
        if member_changes > 0 {
            println!("    {member_changes} with member differences");
        }
        if descendant_changes > 0 {
            println!("    {descendant_changes} with descendant differences");
        }
        if singleton_changes > 0 {
            println!("    {singleton_changes} with singleton_class differences");
        }

        if !extra_in_inc.is_empty() {
            println!("\n    Ancestors in incremental but not reference:");
            let mut sorted: Vec<_> = extra_in_inc.into_iter().collect();
            sorted.sort_by(|a, b| b.1.cmp(&a.1));
            for (name, count) in &sorted {
                println!("      {name} ({count} declarations)");
            }
        }
        if !extra_in_ref.is_empty() {
            println!("\n    Ancestors in reference but not incremental:");
            let mut sorted: Vec<_> = extra_in_ref.into_iter().collect();
            sorted.sort_by(|a, b| b.1.cmp(&a.1));
            for (name, count) in &sorted {
                println!("      {name} ({count} declarations)");
            }
        }
    }

    if !diff.added_definitions.is_empty() {
        println!(
            "\n  Added definitions (in reference only): {}",
            diff.added_definitions.len()
        );
    }
    if !diff.removed_definitions.is_empty() {
        println!(
            "\n  Removed definitions (in incremental only): {}",
            diff.removed_definitions.len()
        );
    }

    if !diff.added_references.is_empty() {
        println!(
            "\n  Added references (in reference only): {}",
            diff.added_references.len()
        );
    }
    if !diff.removed_references.is_empty() {
        println!(
            "\n  Removed references (in incremental only): {}",
            diff.removed_references.len()
        );
    }

    if !diff.added_names.is_empty() {
        println!("\n  Added names (in reference only) ({}):", diff.added_names.len());
        for id in &diff.added_names {
            println!("    + {}", describe_name(reference, *id));
        }
    }
    if !diff.removed_names.is_empty() {
        println!(
            "\n  Removed names (in incremental only) ({}):",
            diff.removed_names.len()
        );
        for id in &diff.removed_names {
            println!("    - {}", describe_name(incremental, *id));
        }
    }
    if !diff.changed_names.is_empty() {
        println!("\n  Changed names ({}):", diff.changed_names.len());
        for id in &diff.changed_names {
            println!("    incremental: {}", describe_name(incremental, *id));
            println!("    reference:   {}", describe_name(reference, *id));
        }
    }
}

fn main() -> Result<(), String> {
    let args = Args::parse();
    let project_path = std::fs::canonicalize(&args.path).map_err(|e| format!("Failed to canonicalize path: {e}"))?;
    let original_ref = git(&args.path, &["rev-parse", "HEAD"])?;

    // Step 1: Full index at base ref
    println!("Checking out {}...", args.base_ref);
    git(&args.path, &["checkout", &args.base_ref])?;

    println!("Building base graph...");
    let (mut incremental_graph, base_duration) = build_graph(&args.path);
    println!(
        "  {} declarations, {} definitions, {} references ({:.2}s)",
        incremental_graph.declarations().len(),
        incremental_graph.definitions().len(),
        incremental_graph.constant_references().len(),
        base_duration.as_secs_f64(),
    );

    // Step 2: Checkout updated ref and do incremental update
    let (modified_files, deleted_files) = changed_files(&project_path, &args.base_ref, &args.updated_ref)?;
    println!(
        "\nChecking out {}... ({} modified, {} deleted)",
        args.updated_ref,
        modified_files.len(),
        deleted_files.len(),
    );
    git(&args.path, &["checkout", &args.updated_ref])?;

    let incremental_start = Instant::now();

    let mut result = IndexResult {
        definition_ids: IdentityHashSet::default(),
        reference_ids: IdentityHashSet::default(),
    };
    for path in &deleted_files {
        let uri = file_uri(path);
        let (def_ids, ref_ids) = incremental_graph.delete_uri(&uri);
        result.definition_ids.extend(def_ids);
        result.reference_ids.extend(ref_ids);
    }

    let (index_result, _) = indexing::index_files(&mut incremental_graph, modified_files);
    result.extend(index_result);

    println!(
        "  {} definitions and {} references scheduled for resolution",
        result.definition_ids.len(),
        result.reference_ids.len(),
    );

    let mut resolver = Resolver::new(&mut incremental_graph);
    resolver.resolve(&result.definition_ids, &result.reference_ids);

    let incremental_duration = incremental_start.elapsed();
    println!(
        "  {} declarations, {} definitions, {} references ({:.2}s)",
        incremental_graph.declarations().len(),
        incremental_graph.definitions().len(),
        incremental_graph.constant_references().len(),
        incremental_duration.as_secs_f64(),
    );

    // Step 3: Full index at updated ref for comparison
    println!("\nBuilding reference graph...");
    let (reference_graph, reference_duration) = build_graph(&args.path);
    println!(
        "  {} declarations, {} definitions, {} references ({:.2}s)",
        reference_graph.declarations().len(),
        reference_graph.definitions().len(),
        reference_graph.constant_references().len(),
        reference_duration.as_secs_f64(),
    );

    // Step 4: Restore original ref
    println!("\nRestoring {}...", &original_ref[..12]);
    git(&args.path, &["checkout", &original_ref])?;

    // Step 5: Compare
    println!("\nComparing incremental vs reference...");
    if let Some(diff) = diff::diff(&incremental_graph, &reference_graph) {
        println!("MISMATCH: incremental resolution diverged from full resolution");
        print_summary(&diff);
        if args.verbose {
            print_verbose_diff(&diff, &incremental_graph, &reference_graph);
        }
        std::process::exit(1);
    } else {
        println!("OK: graphs are identical");
        println!(
            "\nIncremental update was {:.1}x faster than full rebuild",
            reference_duration.as_secs_f64() / incremental_duration.as_secs_f64(),
        );
    }

    Ok(())
}
