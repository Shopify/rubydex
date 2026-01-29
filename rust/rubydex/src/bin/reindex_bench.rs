use std::fmt::Write;
use std::time::{Duration, Instant};

use clap::Parser;

use rubydex::{indexing::ruby_indexer::RubyIndexer, model::graph::Graph, resolution::Resolver};

#[derive(Parser, Debug)]
#[command(
    name = "reindex_bench",
    about = "Benchmark re-indexing and incremental resolution performance (fast path / slow path)"
)]
struct Args {
    #[arg(
        long = "iterations",
        default_value = "100",
        help = "Number of iterations per change type"
    )]
    iterations: usize,

    #[arg(long = "verify", help = "Verify incremental resolution matches full resolution")]
    verify: bool,

    #[arg(long = "compare", help = "Compare fast path vs slow path performance")]
    compare: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ChangeType {
    OffsetOnly,
    ReferenceOnly,
    MemberChanged,
    AncestorChanged,
}

impl std::fmt::Display for ChangeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ChangeType::OffsetOnly => write!(f, "OffsetOnly"),
            ChangeType::ReferenceOnly => write!(f, "ReferenceOnly"),
            ChangeType::MemberChanged => write!(f, "MemberChanged"),
            ChangeType::AncestorChanged => write!(f, "AncestorChanged"),
        }
    }
}

const CHANGE_TYPES: [ChangeType; 4] = [
    ChangeType::OffsetOnly,
    ChangeType::ReferenceOnly,
    ChangeType::MemberChanged,
    ChangeType::AncestorChanged,
];

struct Timing {
    update: Duration,
    resolution: Duration,
    used_fast_path: bool,
}

impl Timing {
    fn total(&self) -> Duration {
        self.update + self.resolution
    }
}

struct SyntheticFile {
    uri: String,
    source: String,
}

const NUM_MIXINS: usize = 2000;
const INHERITANCE_DEPTH: usize = 30;
const MIXINS_PER_LEVEL: usize = 3;
const NUM_REOPEN_FILES: usize = 40000;
const METHODS_PER_REOPEN: usize = 20;
const NUM_INDEPENDENT_HIERARCHIES: usize = 4000;
const CLASSES_PER_HIERARCHY: usize = 30;
const NUM_ANCESTOR_TEST_REOPENS: usize = 100;
const REFS_PER_ANCESTOR_REOPEN: usize = 10;

fn generate_synthetic_graph() -> Vec<SyntheticFile> {
    let mut files = Vec::new();

    let mut mixins_source = String::new();
    for i in 0..NUM_MIXINS {
        let _ = write!(
            mixins_source,
            "module Mixin{i}\n  CONST{i} = {i}\n  def mixin{i}_method; end\nend\n"
        );
    }
    files.push(SyntheticFile {
        uri: "file:///synthetic/mixins.rb".to_string(),
        source: mixins_source,
    });

    let mut hierarchy_source = String::new();
    for level in 0..INHERITANCE_DEPTH {
        let mixin_idx = (level * MIXINS_PER_LEVEL) % NUM_MIXINS;
        let parent = if level == 0 {
            String::new()
        } else {
            format!(" < Level{}", level - 1)
        };
        let _ = write!(
            hierarchy_source,
            "class Level{level}{parent}\n  include Mixin{mixin_idx}\n  def level{level}_method; end\nend\n"
        );
    }
    files.push(SyntheticFile {
        uri: "file:///synthetic/hierarchy.rb".to_string(),
        source: hierarchy_source,
    });

    let deepest_level = INHERITANCE_DEPTH - 1;
    for file_idx in 0..NUM_REOPEN_FILES {
        let mut reopen_source = format!("class Level{deepest_level}\n");
        for method_idx in 0..METHODS_PER_REOPEN {
            let idx = file_idx * METHODS_PER_REOPEN + method_idx;
            let const_idx = idx % NUM_MIXINS;
            let _ = writeln!(reopen_source, "  def reopen_m{idx}; CONST{const_idx}; end");
        }
        reopen_source.push_str("end\n");
        files.push(SyntheticFile {
            uri: format!("file:///synthetic/reopen_{file_idx:03}.rb"),
            source: reopen_source,
        });
    }

    for hier_idx in 0..NUM_INDEPENDENT_HIERARCHIES {
        let mut hier_source = String::new();
        for class_idx in 0..CLASSES_PER_HIERARCHY {
            let parent = if class_idx == 0 {
                String::new()
            } else {
                format!(" < H{hier_idx}C{}", class_idx - 1)
            };
            let _ = write!(
                hier_source,
                "class H{hier_idx}C{class_idx}{parent}\n  def h{hier_idx}c{class_idx}_m; end\nend\n"
            );
        }
        files.push(SyntheticFile {
            uri: format!("file:///synthetic/hier_{hier_idx:03}.rb"),
            source: hier_source,
        });
    }

    for file_idx in 0..NUM_ANCESTOR_TEST_REOPENS {
        let mut reopen_source = String::from("class AncestorTest\n");
        for ref_idx in 0..REFS_PER_ANCESTOR_REOPEN {
            let idx = file_idx * REFS_PER_ANCESTOR_REOPEN + ref_idx;
            let _ = writeln!(reopen_source, "  def ancestor_reopen_m{idx}; ANCESTOR_CONST; end");
        }
        reopen_source.push_str("end\n");
        files.push(SyntheticFile {
            uri: format!("file:///synthetic/ancestor_reopen_{file_idx:03}.rb"),
            source: reopen_source,
        });
    }

    files
}

struct TestFile {
    uri: String,
    base_source: String,
    modified_source: Option<String>,
}

impl TestFile {
    fn new(uri: &str, base_source: &str) -> Self {
        Self {
            uri: uri.to_string(),
            base_source: base_source.to_string(),
            modified_source: None,
        }
    }

    fn with_modified(uri: &str, base_source: &str, modified_source: &str) -> Self {
        Self {
            uri: uri.to_string(),
            base_source: base_source.to_string(),
            modified_source: Some(modified_source.to_string()),
        }
    }
}

struct ChangeInfo {
    uri: String,
    modified_source: String,
    restore_source: String,
}

struct TestFiles {
    offset: TestFile,
    reference: TestFile,
    member: TestFile,
    ancestor: TestFile,
}

impl TestFiles {
    fn new() -> Self {
        Self {
            offset: TestFile::new(
                "file:///synthetic/offset_test.rb",
                r"
class Level29
  OFFSET_CONST = 'value'
  def offset_test_method; end
end
",
            ),

            reference: TestFile::with_modified(
                "file:///synthetic/reference_test.rb",
                r"
class Level29
  def reference_test_method; CONST0; end
end
",
                r"
class Level29
  def reference_test_method; CONST0; CONST1; end
end
",
            ),

            member: TestFile::with_modified(
                "file:///synthetic/member_test.rb",
                r"
class Level29
  def member_test_method; CONST0; end
end
",
                r"
class Level29
  CONST0 = 'shadowed'
  def member_test_method; CONST0; end
end
",
            ),

            ancestor: TestFile::with_modified(
                "file:///synthetic/ancestor_test.rb",
                r"
module AncestorMixin
  ANCESTOR_CONST = 'from_mixin'
end

class AncestorTest
  include AncestorMixin
  ANCESTOR_CONST = 'from_class'
  def method; ANCESTOR_CONST; end
end
",
                r"
module AncestorMixin
  ANCESTOR_CONST = 'from_mixin'
end

class AncestorTest
  prepend AncestorMixin
  ANCESTOR_CONST = 'from_class'
  def method; ANCESTOR_CONST; end
end
",
            ),
        }
    }

    fn change_info(&self, change_type: ChangeType) -> ChangeInfo {
        match change_type {
            ChangeType::OffsetOnly => ChangeInfo {
                uri: self.offset.uri.clone(),
                modified_source: format!("\n\n\n{}", self.offset.base_source),
                restore_source: self.offset.base_source.clone(),
            },
            ChangeType::ReferenceOnly => ChangeInfo {
                uri: self.reference.uri.clone(),
                modified_source: self.reference.modified_source.clone().unwrap(),
                restore_source: self.reference.base_source.clone(),
            },
            ChangeType::MemberChanged => ChangeInfo {
                uri: self.member.uri.clone(),
                modified_source: self.member.modified_source.clone().unwrap(),
                restore_source: self.member.base_source.clone(),
            },
            ChangeType::AncestorChanged => ChangeInfo {
                uri: self.ancestor.uri.clone(),
                modified_source: self.ancestor.modified_source.clone().unwrap(),
                restore_source: self.ancestor.base_source.clone(),
            },
        }
    }

    fn all_files(&self) -> Vec<(&str, &str)> {
        vec![
            (&self.offset.uri, &self.offset.base_source),
            (&self.reference.uri, &self.reference.base_source),
            (&self.member.uri, &self.member.base_source),
            (&self.ancestor.uri, &self.ancestor.base_source),
        ]
    }
}

fn main() {
    let args = Args::parse();

    let synthetic_files = generate_synthetic_graph();
    let test_files = TestFiles::new();

    let mut graph = Graph::new();
    let all_test_files = test_files.all_files();
    print!("Indexing {} files...", synthetic_files.len() + all_test_files.len());
    let start = Instant::now();

    for file in &synthetic_files {
        let mut indexer = RubyIndexer::new(file.uri.clone(), &file.source);
        indexer.index();
        graph.extend(indexer.local_graph());
    }

    for (uri, source) in &all_test_files {
        let mut indexer = RubyIndexer::new((*uri).to_string(), source);
        indexer.index();
        graph.extend(indexer.local_graph());
    }

    let mut resolver = Resolver::new(&mut graph);
    resolver.resolve_all();
    graph.change_set_mut().clear();
    println!(" done in {:?}", start.elapsed());

    println!(
        "Graph stats: {} declarations, {} definitions, {} constant references",
        graph.declarations().len(),
        graph.definitions().len(),
        graph.constant_references().len()
    );

    if args.verify {
        println!("\nVerifying incremental resolution correctness...");
        verify_incremental_resolution(&test_files);
        return;
    }

    if args.compare {
        println!("\nComparing fast path vs slow path performance...");
        compare_fast_slow_paths(&mut graph, &test_files, args.iterations);
        return;
    }

    // Warm-up
    for change_type in CHANGE_TYPES {
        run_iteration(&mut graph, &test_files, change_type);
    }

    // Actual benchmark
    let mut results = Vec::new();
    for change_type in CHANGE_TYPES {
        let timings = run_iterations(&mut graph, &test_files, args.iterations, change_type);
        results.push((change_type, timings));
    }

    print_statistics(&results);
}

fn run_iterations(
    graph: &mut Graph,
    test_files: &TestFiles,
    iterations: usize,
    change_type: ChangeType,
) -> Vec<Timing> {
    let mut timings = Vec::with_capacity(iterations);
    for _ in 0..iterations {
        timings.push(run_iteration(graph, test_files, change_type));
    }
    timings
}

fn run_iteration(graph: &mut Graph, test_files: &TestFiles, change_type: ChangeType) -> Timing {
    let change_info = test_files.change_info(change_type);

    let mut indexer = RubyIndexer::new(change_info.uri.clone(), &change_info.modified_source);
    indexer.index();
    let local_graph = indexer.local_graph();

    let start = Instant::now();
    graph.update(local_graph);
    let update_duration = start.elapsed();

    let requires_full = graph.change_set().requires_full_resolution();
    let refs: Vec<_> = graph.change_set().references_to_resolve().copied().collect();

    let resolution_start = Instant::now();
    let mut resolver = Resolver::new(graph);
    if requires_full {
        resolver.resolve_all();
    } else {
        resolver.resolve_incremental(refs.into_iter());
    }
    let resolution_duration = resolution_start.elapsed();

    graph.change_set_mut().clear();

    // Restore original file
    update_file(graph, &change_info.uri, &change_info.restore_source);

    Timing {
        update: update_duration,
        resolution: resolution_duration,
        used_fast_path: !requires_full,
    }
}

fn update_file(graph: &mut Graph, uri: &str, source: &str) {
    let mut indexer = RubyIndexer::new(uri.to_string(), source);
    indexer.index();
    graph.update(indexer.local_graph());

    let requires_full = graph.change_set().requires_full_resolution();
    let refs: Vec<_> = graph.change_set().references_to_resolve().copied().collect();

    let mut resolver = Resolver::new(graph);
    if requires_full {
        resolver.resolve_all();
    } else {
        resolver.resolve_incremental(refs.into_iter());
    }
    graph.change_set_mut().clear();
}

fn compare_fast_slow_paths(graph: &mut Graph, test_files: &TestFiles, iterations: usize) {
    println!();
    println!("Comparison: Fast Path (incremental) vs Slow Path (full resolution)");
    println!("===================================================================");

    for change_type in CHANGE_TYPES {
        println!();
        println!("{change_type}");
        println!("{}", "-".repeat(format!("{change_type}").len()));

        // Run with fast path (auto-detect)
        let mut fast_times = Vec::new();
        for _ in 0..iterations {
            let timing = run_iteration(graph, test_files, change_type);
            fast_times.push((timing.resolution, timing.used_fast_path));
        }

        // Run with forced slow path
        let mut slow_times = Vec::new();
        for _ in 0..iterations {
            let timing = run_iteration_slow_path(graph, test_files, change_type);
            slow_times.push(timing.resolution);
        }

        let fast_used_fast_path = fast_times.iter().filter(|(_, used)| *used).count();
        let fast_resolution: Vec<Duration> = fast_times.iter().map(|(d, _)| *d).collect();
        let slow_resolution: Vec<Duration> = slow_times;

        println!("  Fast path used: {fast_used_fast_path}/{iterations} iterations");

        let fast_avg = average_duration(&fast_resolution);
        let slow_avg = average_duration(&slow_resolution);
        let speedup = slow_avg.as_secs_f64() / fast_avg.as_secs_f64();

        println!("  Resolution (auto):  avg {}", format_duration(fast_avg));
        println!("  Resolution (full):  avg {}", format_duration(slow_avg));
        println!("  Speedup: {speedup:.2}x");
    }
}

fn run_iteration_slow_path(graph: &mut Graph, test_files: &TestFiles, change_type: ChangeType) -> Timing {
    let change_info = test_files.change_info(change_type);

    let mut indexer = RubyIndexer::new(change_info.uri.clone(), &change_info.modified_source);
    indexer.index();
    let local_graph = indexer.local_graph();

    let start = Instant::now();
    graph.update(local_graph);
    let update_duration = start.elapsed();

    let resolution_start = Instant::now();
    let mut resolver = Resolver::new(graph);
    resolver.resolve_all();
    let resolution_duration = resolution_start.elapsed();

    graph.change_set_mut().clear();

    // Restore original file (using slow path too for consistency)
    let mut indexer = RubyIndexer::new(change_info.uri.clone(), &change_info.restore_source);
    indexer.index();
    graph.update(indexer.local_graph());
    let mut resolver = Resolver::new(graph);
    resolver.resolve_all();
    graph.change_set_mut().clear();

    Timing {
        update: update_duration,
        resolution: resolution_duration,
        used_fast_path: false,
    }
}

#[allow(clippy::cast_precision_loss)]
fn average_duration(durations: &[Duration]) -> Duration {
    let sum: Duration = durations.iter().sum();
    Duration::from_secs_f64(sum.as_secs_f64() / durations.len() as f64)
}

fn print_statistics(results: &[(ChangeType, Vec<Timing>)]) {
    println!();
    println!("Results (fast path / slow path incremental resolution)");
    println!("=======================================================");

    for (change_type, timings) in results {
        println!();
        println!("{change_type}");
        println!("{}", "-".repeat(format!("{change_type}").len()));

        let fast_path_count = timings.iter().filter(|t| t.used_fast_path).count();
        println!("Used fast path: {}/{} iterations", fast_path_count, timings.len());

        let mut update_times: Vec<Duration> = timings.iter().map(|t| t.update).collect();
        let mut resolution_times: Vec<Duration> = timings.iter().map(|t| t.resolution).collect();
        let mut total_times: Vec<Duration> = timings.iter().map(Timing::total).collect();

        update_times.sort_unstable();
        resolution_times.sort_unstable();
        total_times.sort_unstable();

        println!();
        println!("Update time:");
        print_time_stats(&update_times);

        println!();
        println!("Resolution time:");
        print_time_stats(&resolution_times);

        println!();
        println!("Total time per iteration:");
        print_time_stats(&total_times);
    }
}

#[allow(clippy::cast_precision_loss)]
fn print_time_stats(times: &[Duration]) {
    let sum: Duration = times.iter().sum();
    let avg = Duration::from_secs_f64(sum.as_secs_f64() / times.len() as f64);
    let min = times[0];
    let max = times[times.len() - 1];
    let p50 = percentile(times, 50);
    let p95 = percentile(times, 95);

    println!(
        "  avg: {}  min: {}  max: {}",
        format_duration(avg),
        format_duration(min),
        format_duration(max)
    );
    println!("  p50: {}  p95: {}", format_duration(p50), format_duration(p95));
}

fn percentile(sorted_times: &[Duration], p: usize) -> Duration {
    let idx = (p * sorted_times.len() / 100).min(sorted_times.len() - 1);
    sorted_times[idx]
}

fn format_duration(d: Duration) -> String {
    let secs = d.as_secs_f64();
    if secs >= 1.0 {
        format!("{secs:.2}s")
    } else if secs >= 0.001 {
        format!("{:.2}ms", secs * 1000.0)
    } else if secs >= 0.000_001 {
        format!("{:.2}µs", secs * 1_000_000.0)
    } else {
        format!("{:.0}ns", secs * 1_000_000_000.0)
    }
}

fn verify_incremental_resolution(test_files: &TestFiles) {
    use rubydex::model::name::NameRef;

    let mut all_passed = true;

    for change_type in CHANGE_TYPES {
        print!("  {change_type}... ");

        let mut graph = build_base_graph(test_files);
        let change_info = test_files.change_info(change_type);

        // Apply the change using incremental resolution (auto-detect fast/slow path)
        let mut indexer = RubyIndexer::new(change_info.uri.clone(), &change_info.modified_source);
        indexer.index();
        graph.update(indexer.local_graph());

        let requires_full = graph.change_set().requires_full_resolution();
        let refs: Vec<_> = graph.change_set().references_to_resolve().copied().collect();

        let mut resolver = Resolver::new(&mut graph);
        if requires_full {
            resolver.resolve_all();
        } else {
            resolver.resolve_incremental(refs.into_iter());
        }
        graph.change_set_mut().clear();

        let used_fast_path = !requires_full;

        // Count resolutions per declaration name in incremental graph
        let mut incremental_counts: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
        for name_ref in graph.names().values() {
            if let NameRef::Resolved(resolved) = name_ref
                && let Some(decl) = graph.declarations().get(resolved.declaration_id())
            {
                *incremental_counts.entry(decl.name().to_string()).or_default() += name_ref.ref_count();
            }
        }

        // Create fresh graph and run full resolution
        let fresh_graph = build_fresh_graph(test_files, &change_info);

        // Count resolutions per declaration name in fresh graph
        let mut full_counts: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
        for name_ref in fresh_graph.names().values() {
            if let NameRef::Resolved(resolved) = name_ref
                && let Some(decl) = fresh_graph.declarations().get(resolved.declaration_id())
            {
                *full_counts.entry(decl.name().to_string()).or_default() += name_ref.ref_count();
            }
        }

        // Compare counts
        let mut mismatches = 0;
        let all_decls: std::collections::HashSet<_> = incremental_counts.keys().chain(full_counts.keys()).collect();
        for decl_name in all_decls {
            let incr = incremental_counts.get(decl_name).copied().unwrap_or(0);
            let full = full_counts.get(decl_name).copied().unwrap_or(0);
            if incr != full {
                mismatches += 1;
                if mismatches <= 5 {
                    eprintln!("    Mismatch: {decl_name} has {incr} refs (incremental) vs {full} refs (full)");
                }
            }
        }

        if mismatches == 0 {
            let path_type = if used_fast_path { "fast path" } else { "slow path" };
            println!("OK (used {path_type})");
        } else {
            println!("FAILED ({mismatches} mismatches)");
            all_passed = false;
        }
    }

    if all_passed {
        println!("\nAll verifications passed!");
    } else {
        println!("\nSome verifications FAILED!");
        std::process::exit(1);
    }
}

fn build_base_graph(test_files: &TestFiles) -> Graph {
    let mut graph = Graph::new();

    let synthetic_files = generate_synthetic_graph();
    for file in &synthetic_files {
        let mut indexer = RubyIndexer::new(file.uri.clone(), &file.source);
        indexer.index();
        graph.extend(indexer.local_graph());
    }

    for (uri, source) in test_files.all_files() {
        let mut indexer = RubyIndexer::new(uri.to_string(), source);
        indexer.index();
        graph.extend(indexer.local_graph());
    }

    let mut resolver = Resolver::new(&mut graph);
    resolver.resolve_all();
    graph.change_set_mut().clear();

    graph
}

fn build_fresh_graph(test_files: &TestFiles, change_info: &ChangeInfo) -> Graph {
    let mut fresh_graph = Graph::new();

    let synthetic_files = generate_synthetic_graph();
    for file in &synthetic_files {
        let mut indexer = RubyIndexer::new(file.uri.clone(), &file.source);
        indexer.index();
        fresh_graph.extend(indexer.local_graph());
    }

    for (uri, source) in test_files.all_files() {
        let actual_source = if uri == change_info.uri {
            &change_info.modified_source
        } else {
            source
        };
        let mut indexer = RubyIndexer::new(uri.to_string(), actual_source);
        indexer.index();
        fresh_graph.extend(indexer.local_graph());
    }

    let mut resolver = Resolver::new(&mut fresh_graph);
    resolver.resolve_all();
    fresh_graph.change_set_mut().clear();

    fresh_graph
}
