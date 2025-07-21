use index::comments::CommentData;
use index::serde::Encoding;
use rusqlite::{Connection, Result};
use std::{collections::HashMap, fs};

// Testing with 1000 records for storage strategy comparison
static SCALE: usize = 1_000_000;
static ENCODINGS_LIST: [Encoding; 5] = [
    Encoding::JSON,
    Encoding::POSTCARD,
    Encoding::MESSAGEPACK,
    Encoding::BINCODE,
    Encoding::CBOR,
];

// Storage Strategy File Size Comparison Benchmark
// Tests different ways to serialize and store 1000 comments,
// comparing only file sizes on disk
fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Storage Strategy File Size Comparison: {} Records", SCALE);
    println!("===================================================");
    println!("Comparing file sizes for different storage methods\n");

    // Create test data in memory
    let mut indexed_comments = HashMap::new();
    for i in 0..SCALE {
        let entry_name = format!("entry_{}", i);
        let comment = CommentData::new(entry_name.clone(), 200); // Longer comments for realistic data
        indexed_comments.insert(entry_name, comment);
    }
    println!("Created {} comments in memory", SCALE);

    // Create dedicated directory for storage benchmark
    let benchmark_dir = "tmp/storage_benchmark";
    fs::create_dir_all(benchmark_dir).ok();

    // Test SQL storage file sizes
    println!("\n🗄️ SQL STORAGE FILE SIZES:");
    println!("===========================");
    test_sql_storage_file_sizes(&indexed_comments)?;

    // Test serde serialization file sizes
    println!("\n💾 SERDE SERIALIZATION FILE SIZES:");
    println!("===================================");
    let mut storage_results = Vec::new();

    for encoding in ENCODINGS_LIST.iter() {
        let result = test_serde_storage_file_size(&indexed_comments, encoding.clone());
        storage_results.push(result);
    }

    // Display storage efficiency summary
    println!("\n📊 FILE SIZE COMPARISON SUMMARY:");
    println!("=================================");
    display_file_size_summary(&storage_results);

    Ok(())
}

fn test_sql_storage_file_sizes(
    indexed_comments: &HashMap<String, CommentData>,
) -> Result<(), Box<dyn std::error::Error>> {
    // Test SQLite without index
    test_sql_file_size(indexed_comments, false, "SQLite (no index)")?;

    // Test SQLite with index
    test_sql_file_size(indexed_comments, true, "SQLite (with index)")?;

    Ok(())
}

fn test_sql_file_size(
    indexed_comments: &HashMap<String, CommentData>,
    with_index: bool,
    scenario_name: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let db_path = format!(
        "tmp/storage_benchmark/{}_{}.db",
        if with_index { "indexed" } else { "no_index" },
        SCALE
    );

    // Clean setup
    fs::remove_file(&db_path).ok();
    let mut conn = Connection::open(&db_path)?;

    // Setup table
    conn.execute(
        "CREATE TABLE comments (
            id INTEGER PRIMARY KEY,
            comment TEXT,
            entry_name TEXT,
            created_at INTEGER,
            score INTEGER
        )",
        [],
    )?;

    if with_index {
        conn.execute("CREATE INDEX idx_entry_name ON comments(entry_name)", [])?;
    }

    // Insert data
    let tx = conn.transaction()?;
    {
        let mut stmt =
            tx.prepare("INSERT INTO comments (comment, entry_name, created_at, score) VALUES (?1, ?2, ?3, ?4)")?;

        for (i, comment_data) in indexed_comments.values().enumerate() {
            let created_at = 1600000000 + (i * 60) as i64;
            let score = (i % 100 + 1) as i64;

            stmt.execute([
                &comment_data.text,
                &comment_data.entry_name,
                &created_at.to_string(),
                &score.to_string(),
            ])?;
        }
    }
    tx.commit()?;

    // Get file size
    let file_size = fs::metadata(&db_path)?.len();

    println!("{:25}: {:10} bytes", scenario_name, file_size);

    Ok(())
}

#[derive(Debug)]
struct StorageResult {
    encoding: String,
    file_size_bytes: f64,
}

fn test_serde_storage_file_size(indexed_comments: &HashMap<String, CommentData>, encoding: Encoding) -> StorageResult {
    let encoding_name = encoding.to_string();

    // Get file path in our benchmark directory
    let file_path = get_serde_file_path(encoding.clone());

    // Serialize and write to file using custom path
    index::serde::serialize_and_write_to_path(encoding.clone(), indexed_comments, &file_path, false);

    // Get file size
    let file_size = fs::metadata(&file_path).map(|metadata| metadata.len()).unwrap_or(0);

    println!("{:25}: {:10} bytes", encoding_name, file_size);

    StorageResult {
        encoding: encoding_name,
        file_size_bytes: file_size as f64,
    }
}

fn get_serde_file_path(encoding: Encoding) -> String {
    let suffix = match encoding {
        Encoding::JSON => "json",
        Encoding::POSTCARD => "postcard",
        Encoding::MESSAGEPACK => "msgpack",
        Encoding::BINCODE => "bincode",
        Encoding::CBOR => "cbor",
    };
    format!("tmp/storage_benchmark/{}.{}", encoding.to_string(), suffix)
}

fn display_file_size_summary(results: &[StorageResult]) {
    println!("Format               | File Size (bytes) | Compression Ratio");
    println!("---------------------|-------------------|------------------");

    // Find the largest file size for comparison
    let max_size = results.iter().map(|r| r.file_size_bytes).fold(0.0, f64::max);

    // Sort by file size (smallest first)
    let mut sorted_results: Vec<_> = results.iter().collect();
    sorted_results.sort_by(|a, b| a.file_size_bytes.partial_cmp(&b.file_size_bytes).unwrap());

    for result in sorted_results {
        let compression_ratio = if max_size > 0.0 {
            max_size / result.file_size_bytes
        } else {
            1.0
        };

        println!(
            "{:20} | {:15.0} | {:14.2}x",
            result.encoding, result.file_size_bytes, compression_ratio
        );
    }

    println!("\nCompression Ratio = Largest File Size / Current File Size");
    println!("Higher ratios indicate better compression efficiency");
}
