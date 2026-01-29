use std::fmt::Write;
use std::time::{Duration, Instant};

use clap::Parser;

use rubydex::{
    indexing::ruby_indexer::RubyIndexer,
    model::graph::Graph,
    resolution::{ResolutionStats, Resolver},
};

#[derive(Parser, Debug)]
#[command(
    name = "reindex_bench",
    about = "Benchmark re-indexing and incremental resolution performance"
)]
struct Args {
    #[arg(long = "verify", help = "Verify incremental resolution matches full resolution")]
    verify: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ChangeType {
    OffsetOnly,
    MemberChanged,
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
    stats: ResolutionStats,
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

    println!();
    println!("Results (incremental resolution)");
    println!("=================================");

    for change_type in CHANGE_TYPES {
        let timing = run_iteration(&mut graph, &test_files, change_type);
        println!();
        println!("{change_type}:");
        println!(
            "  definitions: {}, references: {}, ancestors: {}",
            timing.stats.definitions_processed, timing.stats.references_processed, timing.stats.ancestors_processed,
        );
        println!(
            "  update: {}, resolution: {}, total: {}",
            format_duration(timing.update),
            format_duration(timing.resolution),
            format_duration(timing.total()),
        );
    }
}

fn run_iteration(graph: &mut Graph, test_files: &TestFiles, change_type: ChangeType) -> Timing {
    let change_info = test_files.change_info(change_type);

    let mut indexer = RubyIndexer::new(change_info.uri.clone(), &change_info.modified_source);
    indexer.index();
    let local_graph = indexer.local_graph();

    let start = Instant::now();
    graph.update(local_graph);
    let update_duration = start.elapsed();

    let resolution_start = Instant::now();
    let mut resolver = Resolver::new(graph);
    let stats = resolver.resolve_all();
    let resolution_duration = resolution_start.elapsed();

    // Restore original file
    update_file(graph, &change_info.uri, &change_info.restore_source);

    Timing {
        update: update_duration,
        resolution: resolution_duration,
        stats,
    }
}

fn update_file(graph: &mut Graph, uri: &str, source: &str) {
    let mut indexer = RubyIndexer::new(uri.to_string(), source);
    indexer.index();
    graph.update(indexer.local_graph());
    let mut resolver = Resolver::new(graph);
    resolver.resolve_all();
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
    let mut all_passed = true;

    for change_type in CHANGE_TYPES {
        print!("  {change_type}... ");

        let mut graph = build_base_graph(test_files);
        let change_info = test_files.change_info(change_type);

        // Apply the change using incremental resolution
        let mut indexer = RubyIndexer::new(change_info.uri.clone(), &change_info.modified_source);
        indexer.index();
        graph.update(indexer.local_graph());
        let mut resolver = Resolver::new(&mut graph);
        resolver.resolve_all();

        // Count resolutions per declaration name in incremental graph
        let mut incremental_counts: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
        for (name_id, name) in graph.names() {
            if let Some(decl_id) = graph.resolved_names().get(name_id)
                && let Some(decl) = graph.declarations().get(decl_id)
            {
                *incremental_counts.entry(decl.name().to_string()).or_default() += name.ref_count();
            }
        }

        // Create fresh graph and run full resolution
        let fresh_graph = build_fresh_graph(test_files, &change_info);

        // Count resolutions per declaration name in fresh graph
        let mut full_counts: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
        for (name_id, name) in fresh_graph.names() {
            if let Some(decl_id) = fresh_graph.resolved_names().get(name_id)
                && let Some(decl) = fresh_graph.declarations().get(decl_id)
            {
                *full_counts.entry(decl.name().to_string()).or_default() += name.ref_count();
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
            println!("OK");
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

    fresh_graph
}
