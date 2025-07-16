use index::comments::CommentData;
use prost::Message;
use std::collections::HashMap;
use std::fs;
use std::time::{Duration, Instant};

// Include the generated prost code
include!(concat!(env!("OUT_DIR"), "/comments.rs"));

const NUM_FILES: usize = 10_000;
const COMMENTS_PER_FILE: usize = 100;
const COMMENT_SIZE: usize = 100;

struct BenchmarkResult {
    format_name: String,
    serialize_time: Duration,
    deserialize_time: Duration,
    total_files_size: u64,
    avg_file_size: f64,
}

fn main() {
    println!("🚀 Large-Scale Serialization Benchmark");
    println!("=======================================\n");
    println!(
        "Testing {} files with {} comments per file ({} chars each)",
        NUM_FILES, COMMENTS_PER_FILE, COMMENT_SIZE
    );
    println!("Total comments: {}", NUM_FILES * COMMENTS_PER_FILE);
    println!();

    // Create output directories
    fs::create_dir_all("tmp/benchmark_results").expect("Failed to create directory");

    let mut results = Vec::new();

    // Benchmark each format
    results.push(benchmark_json());
    results.push(benchmark_messagepack());
    results.push(benchmark_bincode());
    results.push(benchmark_postcard());
    results.push(benchmark_cbor());
    results.push(benchmark_prost());

    // Print results
    print_results(&results);
}

fn generate_file_data(file_id: usize) -> HashMap<String, CommentData> {
    let mut comments = HashMap::new();

    for j in 0..COMMENTS_PER_FILE {
        let entry_name = format!("file_{}_entry_{}", file_id, j);
        let comment = CommentData::new(entry_name.clone(), COMMENT_SIZE);
        comments.insert(entry_name, comment);
    }

    comments
}

fn benchmark_json() -> BenchmarkResult {
    println!("📊 Benchmarking JSON...");

    let start = Instant::now();

    // Serialize all files
    for i in 0..NUM_FILES {
        let data = generate_file_data(i);
        let serialized = serde_json::to_vec(&data).unwrap();
        let path = format!("tmp/benchmark_results/file_{}.json", i);
        fs::write(path, &serialized).expect("Failed to write file");

        if i % 1000 == 0 {
            println!("  Serialized {} files...", i);
        }
    }

    let serialize_time = start.elapsed();

    // Measure file sizes
    let (total_size, avg_size) = measure_file_sizes("json");

    // Deserialize all files
    let start = Instant::now();

    for i in 0..NUM_FILES {
        let path = format!("tmp/benchmark_results/file_{}.json", i);
        let data = fs::read(&path).expect("Failed to read file");
        let _: HashMap<String, CommentData> = serde_json::from_slice(&data).unwrap();

        if i % 1000 == 0 {
            println!("  Deserialized {} files...", i);
        }
    }

    let deserialize_time = start.elapsed();

    BenchmarkResult {
        format_name: "JSON".to_string(),
        serialize_time,
        deserialize_time,
        total_files_size: total_size,
        avg_file_size: avg_size,
    }
}

fn benchmark_messagepack() -> BenchmarkResult {
    println!("📊 Benchmarking MessagePack...");

    let start = Instant::now();

    for i in 0..NUM_FILES {
        let data = generate_file_data(i);
        let serialized = rmp_serde::to_vec(&data).unwrap();
        let path = format!("tmp/benchmark_results/file_{}.msgpack", i);
        fs::write(path, &serialized).expect("Failed to write file");

        if i % 1000 == 0 {
            println!("  Serialized {} files...", i);
        }
    }

    let serialize_time = start.elapsed();
    let (total_size, avg_size) = measure_file_sizes("msgpack");

    let start = Instant::now();

    for i in 0..NUM_FILES {
        let path = format!("tmp/benchmark_results/file_{}.msgpack", i);
        let data = fs::read(&path).expect("Failed to read file");
        let _: HashMap<String, CommentData> = rmp_serde::from_slice(&data).unwrap();

        if i % 1000 == 0 {
            println!("  Deserialized {} files...", i);
        }
    }

    let deserialize_time = start.elapsed();

    BenchmarkResult {
        format_name: "MessagePack".to_string(),
        serialize_time,
        deserialize_time,
        total_files_size: total_size,
        avg_file_size: avg_size,
    }
}

fn benchmark_bincode() -> BenchmarkResult {
    println!("📊 Benchmarking Bincode...");

    let start = Instant::now();

    for i in 0..NUM_FILES {
        let data = generate_file_data(i);
        let serialized = bincode::serialize(&data).unwrap();
        let path = format!("tmp/benchmark_results/file_{}.bincode", i);
        fs::write(path, &serialized).expect("Failed to write file");

        if i % 1000 == 0 {
            println!("  Serialized {} files...", i);
        }
    }

    let serialize_time = start.elapsed();
    let (total_size, avg_size) = measure_file_sizes("bincode");

    let start = Instant::now();

    for i in 0..NUM_FILES {
        let path = format!("tmp/benchmark_results/file_{}.bincode", i);
        let data = fs::read(&path).expect("Failed to read file");
        let _: HashMap<String, CommentData> = bincode::deserialize(&data).unwrap();

        if i % 1000 == 0 {
            println!("  Deserialized {} files...", i);
        }
    }

    let deserialize_time = start.elapsed();

    BenchmarkResult {
        format_name: "Bincode".to_string(),
        serialize_time,
        deserialize_time,
        total_files_size: total_size,
        avg_file_size: avg_size,
    }
}

fn benchmark_postcard() -> BenchmarkResult {
    println!("📊 Benchmarking Postcard...");

    let start = Instant::now();

    for i in 0..NUM_FILES {
        let data = generate_file_data(i);
        let serialized = postcard::to_allocvec(&data).unwrap();
        let path = format!("tmp/benchmark_results/file_{}.postcard", i);
        fs::write(path, &serialized).expect("Failed to write file");

        if i % 1000 == 0 {
            println!("  Serialized {} files...", i);
        }
    }

    let serialize_time = start.elapsed();
    let (total_size, avg_size) = measure_file_sizes("postcard");

    let start = Instant::now();

    for i in 0..NUM_FILES {
        let path = format!("tmp/benchmark_results/file_{}.postcard", i);
        let data = fs::read(&path).expect("Failed to read file");
        let _: HashMap<String, CommentData> = postcard::from_bytes(&data).unwrap();

        if i % 1000 == 0 {
            println!("  Deserialized {} files...", i);
        }
    }

    let deserialize_time = start.elapsed();

    BenchmarkResult {
        format_name: "Postcard".to_string(),
        serialize_time,
        deserialize_time,
        total_files_size: total_size,
        avg_file_size: avg_size,
    }
}

fn benchmark_cbor() -> BenchmarkResult {
    println!("📊 Benchmarking CBOR...");

    let start = Instant::now();

    for i in 0..NUM_FILES {
        let data = generate_file_data(i);
        let mut serialized = Vec::new();
        ciborium::into_writer(&data, &mut serialized).unwrap();
        let path = format!("tmp/benchmark_results/file_{}.cbor", i);
        fs::write(path, &serialized).expect("Failed to write file");

        if i % 1000 == 0 {
            println!("  Serialized {} files...", i);
        }
    }

    let serialize_time = start.elapsed();
    let (total_size, avg_size) = measure_file_sizes("cbor");

    let start = Instant::now();

    for i in 0..NUM_FILES {
        let path = format!("tmp/benchmark_results/file_{}.cbor", i);
        let data = fs::read(&path).expect("Failed to read file");
        let _: HashMap<String, CommentData> = ciborium::from_reader(&data[..]).unwrap();

        if i % 1000 == 0 {
            println!("  Deserialized {} files...", i);
        }
    }

    let deserialize_time = start.elapsed();

    BenchmarkResult {
        format_name: "CBOR".to_string(),
        serialize_time,
        deserialize_time,
        total_files_size: total_size,
        avg_file_size: avg_size,
    }
}

fn benchmark_prost() -> BenchmarkResult {
    println!("📊 Benchmarking Prost (Protobuf)...");

    let start = Instant::now();

    for i in 0..NUM_FILES {
        let data = generate_file_data(i);

        // Convert to prost format
        let mut prost_comments = HashMap::new();
        for (key, comment) in &data {
            let serialized = serde_json::to_string(comment).unwrap();
            let json_value: serde_json::Value = serde_json::from_str(&serialized).unwrap();

            prost_comments.insert(
                key.clone(),
                ProtoCommentData {
                    text: json_value["text"].as_str().unwrap().to_string(),
                    entry_name: json_value["entry_name"].as_str().unwrap().to_string(),
                },
            );
        }

        let prost_data = ProtoCommentCollection {
            comments: prost_comments,
        };

        let serialized = prost_data.encode_to_vec();
        let path = format!("tmp/benchmark_results/file_{}.prost", i);
        fs::write(path, &serialized).expect("Failed to write file");

        if i % 1000 == 0 {
            println!("  Serialized {} files...", i);
        }
    }

    let serialize_time = start.elapsed();
    let (total_size, avg_size) = measure_file_sizes("prost");

    let start = Instant::now();

    for i in 0..NUM_FILES {
        let path = format!("tmp/benchmark_results/file_{}.prost", i);
        let data = fs::read(&path).expect("Failed to read file");
        let _: ProtoCommentCollection = ProtoCommentCollection::decode(&data[..]).unwrap();

        if i % 1000 == 0 {
            println!("  Deserialized {} files...", i);
        }
    }

    let deserialize_time = start.elapsed();

    BenchmarkResult {
        format_name: "Prost (Protobuf)".to_string(),
        serialize_time,
        deserialize_time,
        total_files_size: total_size,
        avg_file_size: avg_size,
    }
}

fn measure_file_sizes(extension: &str) -> (u64, f64) {
    let mut total_size = 0u64;
    let mut file_count = 0;

    for i in 0..NUM_FILES {
        let path = format!("tmp/benchmark_results/file_{}.{}", i, extension);
        if let Ok(metadata) = fs::metadata(&path) {
            total_size += metadata.len();
            file_count += 1;
        }
    }

    let avg_size = if file_count > 0 {
        total_size as f64 / file_count as f64
    } else {
        0.0
    };

    (total_size, avg_size)
}

fn print_results(results: &[BenchmarkResult]) {
    println!();
    println!("📈 Large-Scale Benchmark Results");
    println!("================================");
    println!();

    // Find the fastest times and smallest sizes
    let fastest_serialize = results.iter().min_by_key(|r| r.serialize_time).unwrap();
    let fastest_deserialize = results.iter().min_by_key(|r| r.deserialize_time).unwrap();
    let smallest_total_size = results.iter().min_by_key(|r| r.total_files_size).unwrap();
    let smallest_avg_size = results
        .iter()
        .min_by(|a, b| a.avg_file_size.partial_cmp(&b.avg_file_size).unwrap())
        .unwrap();

    println!(
        "{:<15} {:>12} {:>14} {:>12} {:>15} {:>15} {:>12}",
        "Format", "Serialize", "Deserialize", "Total", "Total Size", "Avg File Size", "Files/sec"
    );
    println!("{}", "-".repeat(105));

    for result in results {
        let total_time = result.serialize_time + result.deserialize_time;
        let files_per_sec = NUM_FILES as f64 / total_time.as_secs_f64();

        let serialize_marker = if result.serialize_time == fastest_serialize.serialize_time {
            " ⚡"
        } else {
            "   "
        };
        let deserialize_marker = if result.deserialize_time == fastest_deserialize.deserialize_time {
            " ⚡"
        } else {
            "   "
        };
        let total_size_marker = if result.total_files_size == smallest_total_size.total_files_size {
            " 🏆"
        } else {
            "   "
        };
        let avg_size_marker = if result.avg_file_size == smallest_avg_size.avg_file_size {
            " 🏆"
        } else {
            "   "
        };

        println!(
            "{:<15} {:>9.2}s{} {:>11.2}s{} {:>9.2}s {:>12.1}MB{} {:>12.0}B{} {:>9.0} f/s",
            result.format_name,
            result.serialize_time.as_secs_f64(),
            serialize_marker,
            result.deserialize_time.as_secs_f64(),
            deserialize_marker,
            total_time.as_secs_f64(),
            result.total_files_size as f64 / 1024.0 / 1024.0,
            total_size_marker,
            result.avg_file_size,
            avg_size_marker,
            files_per_sec
        );
    }

    println!();
    println!("⚡ = Fastest in category");
    println!("🏆 = Smallest size");
    println!();
    println!(
        "Total data processed: {} files, {} comments, {} MB expected data",
        NUM_FILES,
        NUM_FILES * COMMENTS_PER_FILE,
        (NUM_FILES * COMMENTS_PER_FILE * COMMENT_SIZE) as f64 / 1024.0 / 1024.0
    );
}
