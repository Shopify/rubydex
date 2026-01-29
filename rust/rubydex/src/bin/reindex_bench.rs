use std::fmt::Write;
use std::time::{Duration, Instant};

use clap::Parser;

use rubydex::{indexing::ruby_indexer::RubyIndexer, model::graph::Graph, resolution::Resolver};

#[derive(Parser, Debug)]
#[command(
    name = "reindex_bench",
    about = "Benchmark re-indexing and incremental resolution performance"
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

    #[arg(long = "full-only", help = "Only run full resolution (for comparison)")]
    full_only: bool,
}

/// The three core scenarios that can trigger reference re-resolution.
///
/// These map directly to the code paths in `find_affected_references`:
/// - `MemberChanged` → `changed_members` path
/// - `AncestorChanged` → `changed_ancestors` path
/// - (`OffsetOnly` tests that position changes don't trigger re-resolution)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ChangeType {
    /// Position-only change (no semantic change).
    /// Should not trigger any re-resolution since member order is unchanged.
    OffsetOnly,
    /// A constant/class/module is added/removed/reordered in a namespace.
    MemberChanged,
    /// Mixin or superclass changed for a class/module.
    AncestorChanged,
}

impl std::fmt::Display for ChangeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ChangeType::OffsetOnly => write!(f, "OffsetOnly"),
            ChangeType::MemberChanged => write!(f, "MemberChanged"),
            ChangeType::AncestorChanged => write!(f, "AncestorChanged"),
        }
    }
}

const CHANGE_TYPES: [ChangeType; 3] = [
    ChangeType::OffsetOnly,
    ChangeType::MemberChanged,
    ChangeType::AncestorChanged,
];

struct Timing {
    update: Duration,
    resolution: Duration,
    refs_resolved: usize,
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

    // Generate base mixins
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

    // Generate inheritance hierarchy (Level0 < Level1 < ... < Level29)
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

    // Generate files that reopen the deepest class with constant references
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

    // Generate independent class hierarchies
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

    // Generate reopen files for AncestorTest (used by AncestorChanged benchmark)
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
    setup_source: Option<String>,
}

/// Test files for the three core scenarios.
///
/// These files interact with the complex synthetic graph:
/// - Level29 is the deepest class in a 30-level hierarchy
/// - Each level includes a Mixin with constants (CONST0, CONST1, etc.)
/// - 40K reopen files add methods to Level29 that reference these constants
/// - Changes to Level29's members or ancestors affect many references
struct TestFiles {
    /// For `OffsetOnly`: position change without semantic change
    offset: TestFile,
    /// For `MemberChanged`: add CONST0 to Level29, shadowing the inherited one
    member: TestFile,
    /// For `AncestorChanged`: change mixin from include to prepend
    ancestor: TestFile,
}

impl TestFiles {
    fn new() -> Self {
        Self {
            // OffsetOnly: Reopen Level29 with position-only change.
            // No references - just member definitions.
            // Tests that member position changes don't trigger re-resolution.
            offset: TestFile::new(
                "file:///synthetic/offset_test.rb",
                r"
class Level29
  OFFSET_CONST = 'value'
  def offset_test_method; end
end
",
            ),

            // MemberChanged: Add CONST0 to Level29, shadowing the CONST0 from Mixin0.
            // This affects ~400 references to CONST0 in the reopen files.
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

            // AncestorChanged: Change a mixin from include to prepend.
            // This changes the lookup chain and affects ALL references nested in the class.
            // AncestorTest has 100 reopen files with 10 refs each = ~1000 refs affected.
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
                setup_source: None,
            },
            ChangeType::MemberChanged => ChangeInfo {
                uri: self.member.uri.clone(),
                modified_source: self.member.modified_source.clone().unwrap(),
                restore_source: self.member.base_source.clone(),
                setup_source: None,
            },
            ChangeType::AncestorChanged => ChangeInfo {
                uri: self.ancestor.uri.clone(),
                modified_source: self.ancestor.modified_source.clone().unwrap(),
                restore_source: self.ancestor.base_source.clone(),
                setup_source: None,
            },
        }
    }

    fn all_files(&self) -> Vec<(&str, &str)> {
        vec![
            (&self.offset.uri, &self.offset.base_source),
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
        graph.update(indexer.local_graph());
    }

    for (uri, source) in &all_test_files {
        let mut indexer = RubyIndexer::new((*uri).to_string(), source);
        indexer.index();
        graph.update(indexer.local_graph());
    }

    let mut resolver = Resolver::new(&mut graph);
    resolver.resolve_all();
    println!(" done in {:?}", start.elapsed());

    println!(
        "Graph stats: {} declarations, {} definitions, {} names",
        graph.declarations().len(),
        graph.definitions().len(),
        graph.names().len()
    );

    if args.verify {
        println!("\nVerifying incremental resolution correctness...");
        verify_incremental_resolution(&mut graph, &test_files);
        return;
    }

    // Warm-up
    for change_type in CHANGE_TYPES {
        run_iteration(&mut graph, &test_files, change_type, args.full_only);
    }

    // Actual benchmark
    let mut results = Vec::new();
    for change_type in CHANGE_TYPES {
        let timings = run_iterations(&mut graph, &test_files, args.iterations, change_type, args.full_only);
        results.push((change_type, timings));
    }

    print_statistics(&results, args.full_only);
}

fn run_iterations(
    graph: &mut Graph,
    test_files: &TestFiles,
    iterations: usize,
    change_type: ChangeType,
    full_only: bool,
) -> Vec<Timing> {
    let mut timings = Vec::with_capacity(iterations);
    for _ in 0..iterations {
        timings.push(run_iteration(graph, test_files, change_type, full_only));
    }
    timings
}

fn run_iteration(graph: &mut Graph, test_files: &TestFiles, change_type: ChangeType, full_only: bool) -> Timing {
    let change_info = test_files.change_info(change_type);

    // Apply setup if needed (e.g., RemoveConstant needs to add the constant first)
    if let Some(setup_source) = &change_info.setup_source {
        update_file(graph, &change_info.uri, setup_source, full_only);
    }

    let mut indexer = RubyIndexer::new(change_info.uri.clone(), &change_info.modified_source);
    indexer.index();
    let local_graph = indexer.local_graph();

    let (update_duration, resolution_duration, refs_resolved) = if full_only {
        let start = Instant::now();
        graph.update(local_graph);
        let update_duration = start.elapsed();

        let resolution_start = Instant::now();
        let mut resolver = Resolver::new(graph);
        resolver.resolve_all();
        let resolution_duration = resolution_start.elapsed();

        (update_duration, resolution_duration, 0)
    } else {
        let start = Instant::now();
        let refs_resolved = graph.incremental_update(local_graph);
        let resolution_duration = start.elapsed();

        (Duration::ZERO, resolution_duration, refs_resolved)
    };

    // Restore original file
    update_file(graph, &change_info.uri, &change_info.restore_source, full_only);

    Timing {
        update: update_duration,
        resolution: resolution_duration,
        refs_resolved,
    }
}

fn update_file(graph: &mut Graph, uri: &str, source: &str, full_only: bool) {
    let mut indexer = RubyIndexer::new(uri.to_string(), source);
    indexer.index();
    if full_only {
        graph.update(indexer.local_graph());
        let mut resolver = Resolver::new(graph);
        resolver.resolve_all();
    } else {
        graph.incremental_update(indexer.local_graph());
    }
}

fn print_statistics(results: &[(ChangeType, Vec<Timing>)], full_only: bool) {
    println!();
    println!(
        "Results ({})",
        if full_only {
            "full resolution"
        } else {
            "incremental resolution"
        }
    );
    println!("=======");

    for (change_type, timings) in results {
        println!();
        println!("{change_type}");
        println!("{}", "-".repeat(format!("{change_type}").len()));

        let mut update_times: Vec<Duration> = timings.iter().map(|t| t.update).collect();
        let mut resolution_times: Vec<Duration> = timings.iter().map(|t| t.resolution).collect();
        let mut total_times: Vec<Duration> = timings.iter().map(Timing::total).collect();

        update_times.sort_unstable();
        resolution_times.sort_unstable();
        total_times.sort_unstable();

        if !full_only {
            let avg_refs: usize = timings.iter().map(|t| t.refs_resolved).sum::<usize>() / timings.len();
            println!();
            println!("References re-resolved (avg): {avg_refs}");
        }

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

#[allow(clippy::too_many_lines)]
fn verify_incremental_resolution(_graph: &mut Graph, test_files: &TestFiles) {
    use rubydex::model::name::NameRef;

    let mut all_passed = true;

    for change_type in CHANGE_TYPES {
        print!("  {change_type}... ");

        // Build a fresh base graph for each test to ensure isolation
        let mut graph = build_base_graph(test_files);
        let change_info = test_files.change_info(change_type);

        // Apply setup if needed
        if let Some(setup_source) = &change_info.setup_source {
            apply_incremental_update(&mut graph, &change_info.uri, setup_source);
        }

        // Apply the change using incremental resolution
        let refs_resolved = apply_incremental_update(&mut graph, &change_info.uri, &change_info.modified_source);

        let mut incremental_counts: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
        for name_ref in graph.names().values() {
            if let NameRef::Resolved(resolved) = name_ref
                && let Some(decl) = graph.declarations().get(resolved.declaration_id())
            {
                *incremental_counts.entry(decl.name().to_string()).or_default() += name_ref.ref_count();
            }
        }

        let fresh_graph = build_fresh_graph(test_files, &change_info);

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
            println!("OK ({refs_resolved} refs re-resolved)");
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

fn apply_incremental_update(graph: &mut Graph, uri: &str, source: &str) -> usize {
    let mut indexer = RubyIndexer::new(uri.to_string(), source);
    indexer.index();
    graph.incremental_update(indexer.local_graph())
}

fn build_base_graph(test_files: &TestFiles) -> Graph {
    let mut graph = Graph::new();

    let synthetic_files = generate_synthetic_graph();
    for file in &synthetic_files {
        let mut indexer = RubyIndexer::new(file.uri.clone(), &file.source);
        indexer.index();
        graph.update(indexer.local_graph());
    }

    for (uri, source) in test_files.all_files() {
        let mut indexer = RubyIndexer::new(uri.to_string(), source);
        indexer.index();
        graph.update(indexer.local_graph());
    }

    let mut resolver = Resolver::new(&mut graph);
    resolver.resolve_all();

    graph
}

fn build_fresh_graph(test_files: &TestFiles, change_info: &ChangeInfo) -> Graph {
    let mut fresh_graph = Graph::new();

    // Index synthetic files
    let synthetic_files = generate_synthetic_graph();
    for file in &synthetic_files {
        let mut indexer = RubyIndexer::new(file.uri.clone(), &file.source);
        indexer.index();
        fresh_graph.update(indexer.local_graph());
    }

    // Index test files, using modified_source for the target file
    for (uri, source) in test_files.all_files() {
        let actual_source = if *uri == change_info.uri {
            &change_info.modified_source
        } else {
            source
        };
        let mut indexer = RubyIndexer::new(uri.to_string(), actual_source);
        indexer.index();
        fresh_graph.update(indexer.local_graph());
    }

    let mut resolver = Resolver::new(&mut fresh_graph);
    resolver.resolve_all();

    fresh_graph
}
