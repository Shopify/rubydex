use std::{
    collections::HashSet,
    error::Error,
    io,
    path::PathBuf,
    process::ExitCode,
    time::{Duration, Instant},
};

use clap::Parser;
use rubydex::{
    indexing::{self, IndexerBackend, LanguageId},
    integrity, listing,
    model::{
        declaration::{Ancestor, Ancestors, Namespace},
        graph::{Graph, Unit},
        ids::{ConstantReferenceId, DeclarationId, DefinitionId},
        name::NameRef,
    },
    resolution::Resolver,
};

const MIXIN_A: &str = "RubydexIncrementalBenchmarkMixinA";
const MIXIN_B: &str = "RubydexIncrementalBenchmarkMixinB";
const MIXIN_URI: &str = "file:///__rubydex_incremental_benchmark__/mixins.rb";

type Result<T> = std::result::Result<T, Box<dyn Error>>;

#[derive(Debug, Parser)]
#[command(about = "Benchmark incremental resolution against an existing workspace graph")]
struct Args {
    /// Added by `cargo bench` for harness-less benchmarks.
    #[arg(long = "bench", hide = true)]
    _bench: bool,

    /// Workspace root. Used for rubydex.toml and reporting.
    workspace: PathBuf,

    /// File or directory to index. May be repeated. Defaults to the workspace root.
    #[arg(long = "index-path")]
    index_paths: Vec<PathBuf>,

    /// Top-level class name to benchmark. May be repeated.
    #[arg(long = "target")]
    targets: Vec<String>,

    /// Print light/typical/heavy class candidates and exit unless --target is also supplied.
    #[arg(long)]
    list_candidates: bool,

    /// Print unresolved graph state after the initial resolution and exit.
    #[arg(long)]
    resolution_stats: bool,

    /// Number of unmeasured A -> B -> A warmup cycles.
    #[arg(long, default_value_t = 5)]
    warmup: usize,

    /// Number of measured A -> B -> A cycles.
    #[arg(long, default_value_t = 30)]
    cycles: usize,
}

#[derive(Clone)]
struct Candidate {
    id: DeclarationId,
    name: String,
    descendants: usize,
    ancestor_depth: usize,
    definitions: usize,
    direct_references: usize,
}

#[derive(Clone, Copy)]
struct Sample {
    index: Duration,
    merge: Duration,
    resolve: Duration,
    total: Duration,
}

struct PendingWorkSummary {
    total: usize,
    raw_definitions: usize,
    raw_constant_references: usize,
    raw_ancestors: usize,
    definitions: HashSet<DefinitionId>,
    constant_references: HashSet<ConstantReferenceId>,
    ancestors: HashSet<DeclarationId>,
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("incremental resolution benchmark failed: {error}");
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<()> {
    let args = Args::parse();
    if args.cycles == 0 {
        return Err(input_error("--cycles must be greater than zero"));
    }

    let workspace = std::fs::canonicalize(&args.workspace)?;
    if !workspace.is_dir() {
        return Err(input_error(format!(
            "workspace is not a directory: {}",
            workspace.display()
        )));
    }

    let index_paths = if args.index_paths.is_empty() {
        vec![workspace.clone()]
    } else {
        args.index_paths
    };

    let mut graph = Graph::new();
    graph.set_workspace_path(workspace.clone());
    if let Err(error) = graph.load_config(None) {
        eprintln!("warning: {error}");
    }

    let listing_started = Instant::now();
    let (file_paths, listing_errors) = listing::collect_file_paths(
        index_paths
            .iter()
            .map(|path| path.to_string_lossy().into_owned())
            .collect(),
        &graph.excluded_patterns(),
    );
    ensure_no_errors("listing", &listing_errors)?;
    let listing_elapsed = listing_started.elapsed();

    let indexed_file_count = file_paths.len();
    let indexing_started = Instant::now();
    let indexing_errors = indexing::index_files(&mut graph, file_paths, IndexerBackend::RubyIndexer);
    ensure_no_errors("indexing", &indexing_errors)?;
    let indexing_elapsed = indexing_started.elapsed();

    let resolution_started = Instant::now();
    Resolver::new(&mut graph).resolve();
    let resolution_elapsed = resolution_started.elapsed();

    println!("Workspace: {}", workspace.display());
    println!("Indexed paths: {}", index_paths.len());
    println!("Indexed files: {indexed_file_count}");
    println!("Declarations: {}", graph.declarations().len());
    println!(
        "Initial build: listing {:.3}s, indexing {:.3}s, resolution {:.3}s",
        listing_elapsed.as_secs_f64(),
        indexing_elapsed.as_secs_f64(),
        resolution_elapsed.as_secs_f64(),
    );

    if args.resolution_stats {
        print_resolution_stats(&mut graph);
        return Ok(());
    }

    let candidates = collect_candidates(&graph, &workspace);
    if candidates.is_empty() {
        return Err(input_error("no class with complete ancestors was found"));
    }

    if args.list_candidates {
        print_candidates(&graph, &candidates);
        if args.targets.is_empty() {
            return Ok(());
        }
    }

    if args.targets.is_empty() {
        return Err(input_error("pass --list-candidates or at least one --target CLASS"));
    }
    if let Some(target) = args.targets.iter().find(|target| target.contains("::")) {
        return Err(input_error(format!(
            "target must be a top-level class so the virtual reopen is unambiguous: {target}"
        )));
    }

    ensure_synthetic_names_are_available(&graph)?;
    let baseline_declaration_count = graph.declarations().len();
    let baseline_integrity = integrity_messages(&graph);

    benchmark_noop(&mut graph, args.warmup, args.cycles);

    let mixin_source = format!("module {MIXIN_A}\nend\nmodule {MIXIN_B}\nend\n");
    apply_source(&mut graph, MIXIN_URI, &mixin_source);

    for (index, target) in args.targets.iter().enumerate() {
        benchmark_target(&mut graph, target, index, args.warmup, args.cycles)?;
    }

    if graph.delete_document(MIXIN_URI).is_none() {
        return Err(input_error("synthetic mixin document disappeared before cleanup"));
    }
    Resolver::new(&mut graph).resolve();

    if graph.declarations().len() != baseline_declaration_count {
        return Err(input_error(format!(
            "cleanup changed declaration count: expected {baseline_declaration_count}, got {}",
            graph.declarations().len(),
        )));
    }

    let final_integrity = integrity_messages(&graph);
    if final_integrity != baseline_integrity {
        return Err(input_error("cleanup changed graph integrity errors"));
    }

    Ok(())
}

fn print_resolution_stats(graph: &mut Graph) {
    let (unresolved_name_count, unresolved_name_uses) = graph
        .names()
        .values()
        .filter(|name| matches!(name, NameRef::Unresolved(_)))
        .fold((0, 0_u64), |(count, uses), name| {
            (count + 1, uses + u64::from(name.ref_count()))
        });

    let unresolved_constant_references = graph
        .constant_references()
        .values()
        .filter(|reference| matches!(graph.names().get(reference.name_id()), Some(NameRef::Unresolved(_))))
        .count();
    let unresolved_named_definitions = graph
        .definitions()
        .values()
        .filter(|definition| {
            definition
                .name_id()
                .is_some_and(|name_id| matches!(graph.names().get(name_id), Some(NameRef::Unresolved(_))))
        })
        .count();
    let unlinked_definitions = graph
        .definitions()
        .values()
        .filter(|definition| graph.definition_to_declaration_id(definition).is_none())
        .count();

    let mut complete_ancestors = 0;
    let mut cyclic_ancestors = 0;
    let mut partial_ancestors = 0;
    let mut partial_ancestor_entries = 0;
    for namespace in graph
        .declarations()
        .values()
        .filter_map(|declaration| declaration.as_namespace())
    {
        match namespace.ancestors() {
            Ancestors::Complete(_) => complete_ancestors += 1,
            Ancestors::Cyclic(_) => cyclic_ancestors += 1,
            Ancestors::Partial(ancestors) => {
                partial_ancestors += 1;
                partial_ancestor_entries += ancestors
                    .iter()
                    .filter(|ancestor| matches!(ancestor, Ancestor::Partial(_)))
                    .count();
            }
        }
    }

    let pending = take_pending_work_summary(graph);

    println!();
    println!("Post-resolution graph state");
    println!(
        "Names: {} total, {} unresolved unique, {unresolved_name_uses} unresolved uses",
        graph.names().len(),
        unresolved_name_count,
    );
    println!(
        "Constant references: {} total, {unresolved_constant_references} unresolved",
        graph.constant_references().len(),
    );
    println!(
        "Definitions: {} total, {unresolved_named_definitions} unresolved named, {unlinked_definitions} unlinked",
        graph.definitions().len(),
    );
    println!(
        "Namespace ancestors: {complete_ancestors} complete, {cyclic_ancestors} cyclic, \
         {partial_ancestors} partial ({partial_ancestor_entries} partial entries)",
    );
    println!(
        "Pending work: {} raw; definitions {} raw/{} unique, constant refs {} raw/{} unique, ancestors {} raw/{} unique",
        pending.total,
        pending.raw_definitions,
        pending.definitions.len(),
        pending.raw_constant_references,
        pending.constant_references.len(),
        pending.raw_ancestors,
        pending.ancestors.len(),
    );
}

fn take_pending_work_summary(graph: &mut Graph) -> PendingWorkSummary {
    // This diagnostic mode exits immediately, so draining pending work does not affect a benchmark run.
    let pending_work = graph.take_pending_work();
    let mut summary = PendingWorkSummary {
        total: pending_work.len(),
        raw_definitions: 0,
        raw_constant_references: 0,
        raw_ancestors: 0,
        definitions: HashSet::new(),
        constant_references: HashSet::new(),
        ancestors: HashSet::new(),
    };

    for unit in pending_work {
        match unit {
            Unit::Definition(id) => {
                summary.raw_definitions += 1;
                summary.definitions.insert(id);
            }
            Unit::ConstantRef(id) => {
                summary.raw_constant_references += 1;
                summary.constant_references.insert(id);
            }
            Unit::Ancestors(id) => {
                summary.raw_ancestors += 1;
                summary.ancestors.insert(id);
            }
        }
    }

    summary
}

fn collect_candidates(graph: &Graph, workspace: &std::path::Path) -> Vec<Candidate> {
    let excluded = ["BasicObject", "Object", "Module", "Class"];
    let mut candidates: Vec<Candidate> = graph
        .declarations()
        .iter()
        .filter_map(|(id, declaration)| {
            let namespace = declaration.as_namespace()?;
            if !matches!(namespace, Namespace::Class(_))
                || !namespace.has_complete_ancestors()
                || namespace.definitions().is_empty()
                || !is_defined_in_workspace(graph, namespace, workspace)
                || excluded.contains(&declaration.name())
                || declaration.name().contains("::")
                || declaration.name().contains('<')
                || declaration.name().contains('#')
            {
                return None;
            }

            Some(Candidate {
                id: *id,
                name: declaration.name().to_string(),
                descendants: namespace.descendants().len(),
                ancestor_depth: namespace.ancestors().iter().count(),
                definitions: namespace.definitions().len(),
                direct_references: namespace.references().len(),
            })
        })
        .collect();

    candidates.sort_unstable_by(|a, b| (a.descendants, &a.name).cmp(&(b.descendants, &b.name)));
    candidates
}

fn is_defined_in_workspace(graph: &Graph, namespace: &Namespace, workspace: &std::path::Path) -> bool {
    namespace.definitions().iter().any(|id| {
        graph
            .definitions()
            .get(id)
            .and_then(|definition| graph.documents().get(definition.uri_id()))
            .and_then(rubydex::model::document::Document::file_path)
            .is_some_and(|path| path.starts_with(workspace))
    })
}

fn print_candidates(graph: &Graph, candidates: &[Candidate]) {
    println!();
    println!("Top-level class candidates by transitive descendant count");
    println!("bucket\tdescendants\tancestor_depth\tdefinitions\tdirect_refs\tsubtree_refs\tclass");

    let structural: Vec<&Candidate> = candidates
        .iter()
        .filter(|candidate| candidate.descendants > 1)
        .collect();
    let mut printed = HashSet::new();

    print_candidate(graph, "light", &candidates[0], &mut printed);

    let population: Vec<&Candidate> = if structural.is_empty() {
        candidates.iter().collect()
    } else {
        structural
    };
    for (label, percentile) in [
        ("typical_structural", 50),
        ("heavy", 90),
        ("very_heavy", 99),
        ("max", 100),
    ] {
        let index = (population.len() - 1) * percentile / 100;
        print_candidate(graph, label, population[index], &mut printed);
    }

    println!();
    println!("Top workspace top-level classes by transitive descendant count");
    println!("descendants\tancestor_depth\tdefinitions\tdirect_refs\tsubtree_refs\tclass");
    for candidate in candidates.iter().rev().take(10) {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}",
            candidate.descendants,
            candidate.ancestor_depth,
            candidate.definitions,
            candidate.direct_references,
            subtree_reference_count(graph, candidate),
            candidate.name,
        );
    }
}

fn print_candidate(graph: &Graph, label: &str, candidate: &Candidate, printed: &mut HashSet<DeclarationId>) {
    if printed.insert(candidate.id) {
        println!(
            "{label}\t{}\t{}\t{}\t{}\t{}\t{}",
            candidate.descendants,
            candidate.ancestor_depth,
            candidate.definitions,
            candidate.direct_references,
            subtree_reference_count(graph, candidate),
            candidate.name,
        );
    }
}

fn subtree_reference_count(graph: &Graph, candidate: &Candidate) -> usize {
    let Some(target) = graph
        .declarations()
        .get(&candidate.id)
        .and_then(|declaration| declaration.as_namespace())
    else {
        return 0;
    };

    std::iter::once(&candidate.id)
        .chain(target.descendants())
        .filter_map(|id| {
            graph
                .declarations()
                .get(id)
                .and_then(|declaration| declaration.as_namespace())
        })
        .map(|namespace| namespace.references().len())
        .sum()
}

fn benchmark_noop(graph: &mut Graph, warmup: usize, cycles: usize) {
    for _ in 0..warmup {
        Resolver::new(graph).resolve();
    }

    let mut durations = Vec::with_capacity(cycles);
    for _ in 0..cycles {
        let started = Instant::now();
        Resolver::new(graph).resolve();
        durations.push(started.elapsed());
    }

    println!();
    println!("No-op resolve baseline");
    println!("Warmup cycles: {warmup}, measured cycles: {cycles}");
    print_duration_summary("noop", "resolve", durations);
}

fn benchmark_target(graph: &mut Graph, target: &str, index: usize, warmup: usize, cycles: usize) -> Result<()> {
    let target_id = DeclarationId::from(target);
    let (original_ancestors, original_descendants, descendant_count) = {
        let declaration = graph
            .declarations()
            .get(&target_id)
            .ok_or_else(|| input_error(format!("target class does not exist: {target}")))?;
        let namespace = declaration
            .as_namespace()
            .filter(|namespace| matches!(namespace, Namespace::Class(_)))
            .ok_or_else(|| input_error(format!("target is not a class: {target}")))?;
        (
            namespace.ancestors().iter().copied().collect::<Vec<_>>(),
            namespace.descendants().iter().copied().collect::<HashSet<_>>(),
            namespace.descendants().len(),
        )
    };

    let uri = format!("file:///__rubydex_incremental_benchmark__/target_{index}.rb");
    let source_a = format!("class {target}\n  include ::{MIXIN_A}\nend\n");
    let source_b = format!("class {target}\n  include ::{MIXIN_B}\nend\n");
    debug_assert_eq!(source_a.len(), source_b.len());

    apply_source(graph, &uri, &source_a);
    assert_mixin(graph, target, target_id, MIXIN_A, MIXIN_B)?;

    for _ in 0..warmup {
        apply_source(graph, &uri, &source_b);
        apply_source(graph, &uri, &source_a);
    }

    let mut a_to_b = Vec::with_capacity(cycles);
    let mut b_to_a = Vec::with_capacity(cycles);
    for _ in 0..cycles {
        a_to_b.push(apply_source(graph, &uri, &source_b));
        b_to_a.push(apply_source(graph, &uri, &source_a));
    }

    assert_mixin(graph, target, target_id, MIXIN_A, MIXIN_B)?;

    println!();
    println!("Target: {target}");
    println!("Transitive descendants: {descendant_count}");
    println!("Warmup cycles: {warmup}, measured cycles: {cycles}");
    print_samples("A_to_B", &a_to_b);
    print_samples("B_to_A", &b_to_a);

    if graph.delete_document(&uri).is_none() {
        return Err(input_error(format!("synthetic target document disappeared: {uri}")));
    }
    Resolver::new(graph).resolve();

    let restored = graph
        .declarations()
        .get(&target_id)
        .and_then(|declaration| declaration.as_namespace())
        .ok_or_else(|| input_error(format!("target class disappeared during cleanup: {target}")))?;
    let restored_ancestors: Vec<Ancestor> = restored.ancestors().iter().copied().collect();

    let descendants_restored = restored.descendants().len() == original_descendants.len()
        && restored
            .descendants()
            .iter()
            .all(|id| original_descendants.contains(id));
    if restored_ancestors != original_ancestors || !descendants_restored {
        return Err(input_error(format!(
            "target graph state was not restored after benchmark: {target}"
        )));
    }

    Ok(())
}

fn apply_source(graph: &mut Graph, uri: &str, source: &str) -> Sample {
    let total_started = Instant::now();

    let index_started = Instant::now();
    let local_graph =
        indexing::build_local_graph(uri.to_string(), source, &LanguageId::Ruby, IndexerBackend::RubyIndexer);
    let index = index_started.elapsed();

    let merge_started = Instant::now();
    graph.consume_document_changes(local_graph);
    let merge = merge_started.elapsed();

    let resolve_started = Instant::now();
    Resolver::new(graph).resolve();
    let resolve = resolve_started.elapsed();

    Sample {
        index,
        merge,
        resolve,
        total: total_started.elapsed(),
    }
}

fn assert_mixin(graph: &Graph, target: &str, target_id: DeclarationId, expected: &str, absent: &str) -> Result<()> {
    let expected_id = DeclarationId::from(expected);
    let absent_id = DeclarationId::from(absent);
    let namespace = graph
        .declarations()
        .get(&target_id)
        .and_then(|declaration| declaration.as_namespace())
        .ok_or_else(|| input_error("target class disappeared while applying synthetic mixin"))?;
    let ancestor_ids: HashSet<DeclarationId> = namespace
        .ancestors()
        .iter()
        .filter_map(|ancestor| match ancestor {
            Ancestor::Complete(id) => Some(*id),
            Ancestor::Partial(_) => None,
        })
        .collect();

    if !ancestor_ids.contains(&expected_id) || ancestor_ids.contains(&absent_id) {
        let mut ancestor_names: Vec<&str> = ancestor_ids
            .iter()
            .filter_map(|id| {
                graph
                    .declarations()
                    .get(id)
                    .map(rubydex::model::declaration::Declaration::name)
            })
            .collect();
        ancestor_names.sort_unstable();
        return Err(input_error(format!(
            "target {target} ancestor chain did not switch from {absent} to {expected}; ancestors: {}",
            ancestor_names.join(", "),
        )));
    }

    Ok(())
}

fn print_samples(direction: &str, samples: &[Sample]) {
    for (phase, durations) in [
        ("index", samples.iter().map(|sample| sample.index).collect()),
        ("merge", samples.iter().map(|sample| sample.merge).collect()),
        ("resolve", samples.iter().map(|sample| sample.resolve).collect()),
        ("total", samples.iter().map(|sample| sample.total).collect()),
    ] {
        print_duration_summary(direction, phase, durations);
    }
}

fn print_duration_summary(direction: &str, phase: &str, mut durations: Vec<Duration>) {
    durations.sort_unstable();
    let median = durations[(durations.len() - 1) / 2];
    let p95 = durations[(durations.len() - 1) * 95 / 100];
    let min = durations[0];
    let max = durations[durations.len() - 1];

    println!(
        "RESULT direction={direction} phase={phase} median_ms={:.3} p95_ms={:.3} min_ms={:.3} max_ms={:.3}",
        milliseconds(median),
        milliseconds(p95),
        milliseconds(min),
        milliseconds(max),
    );
}

fn milliseconds(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1_000.0
}

fn ensure_synthetic_names_are_available(graph: &Graph) -> Result<()> {
    for name in [MIXIN_A, MIXIN_B] {
        if graph.declarations().contains_key(&DeclarationId::from(name)) {
            return Err(input_error(format!("synthetic benchmark name already exists: {name}")));
        }
    }
    Ok(())
}

fn ensure_no_errors(stage: &str, errors: &[rubydex::errors::Errors]) -> Result<()> {
    if errors.is_empty() {
        return Ok(());
    }

    for error in errors {
        eprintln!("{stage}: {error}");
    }
    Err(input_error(format!("{stage} produced {} error(s)", errors.len())))
}

fn integrity_messages(graph: &Graph) -> Vec<String> {
    let mut messages: Vec<String> = integrity::check_integrity(graph)
        .into_iter()
        .map(|error| error.to_string())
        .collect();
    messages.sort_unstable();
    messages
}

fn input_error(message: impl Into<String>) -> Box<dyn Error> {
    io::Error::other(message.into()).into()
}
