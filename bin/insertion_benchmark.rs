use index::comments::CommentData;
use index::serde::Encoding;
use rusqlite::{Connection, Result};
use std::{collections::HashMap, time::Instant};

// Inserting 1M records consistently with reading benchmark
static SCALE: usize = 1_000_000;
static ENCODINGS_LIST: [Encoding; 6] = [
    Encoding::JSON,
    Encoding::POSTCARD,
    Encoding::MESSAGEPACK,
    Encoding::BINCODE,
    Encoding::CBOR,
    Encoding::PROST,
];

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Write Performance Benchmark: 1M Records");
    println!("========================================");
    println!("Comparing SQL vs Serde serialization methods");

    // Create test data in memory (consistent with reading benchmark)
    let mut indexed_comments = HashMap::new();
    for i in 0..SCALE {
        let entry_name = format!("entry_{}", i);
        let comment = CommentData::new(entry_name.clone(), 100);
        indexed_comments.insert(entry_name, comment);
    }
    println!("Created {} comments in memory", SCALE);

    // Test SQL insertions (both with and without index)
    println!("\n🗄️ SQL INSERTION TESTS:");
    println!("=======================");

    println!("\n🚫 WITHOUT entry_name INDEX:");
    println!("-----------------------------");
    test_sql_insertion_scenario(&indexed_comments, false)?;

    println!("\n✅ WITH entry_name INDEX:");
    println!("--------------------------");
    test_sql_insertion_scenario(&indexed_comments, true)?;

    println!("\n🗂️ SHARDED TABLES:");
    println!("------------------");
    test_sharded_sql_insertion(&indexed_comments)?;

    // Test serde serializations
    println!("\n💾 SERDE SERIALIZATION TESTS:");
    println!("=============================");
    for encoding in ENCODINGS_LIST.iter() {
        test_serde_serialization(&indexed_comments, encoding.clone());
        test_serde_serialization_batched(&indexed_comments, encoding.clone());
    }

    Ok(())
}

fn test_sql_insertion_scenario(
    indexed_comments: &HashMap<String, CommentData>,
    with_indexes: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let scenario_name = if with_indexes { "indexed" } else { "no_indexes" };
    let db_path = format!("tmp/write_benchmark_{}_{}.db", scenario_name, SCALE);

    // Clean setup for each test
    std::fs::remove_file(&db_path).ok();
    let conn = Connection::open(&db_path)?;

    setup_table(&conn)?;

    if with_indexes {
        create_indexes(&conn)?;
    }

    // Test transaction batches with size 100 (consistent with reading benchmark)
    test_sql_insertion_batch_100(&conn, indexed_comments, scenario_name)?;

    Ok(())
}

fn test_serde_serialization(indexed_comments: &HashMap<String, CommentData>, encoding: Encoding) {
    let encoding_name = encoding.to_string();
    println!("\nTesting {} serialization:", encoding_name);

    let start = Instant::now();
    index::serde::serialize_and_write(encoding, indexed_comments);
    let duration = start.elapsed();

    let rate = SCALE as f64 / duration.as_secs_f64();
    println!(
        "{} serialization    ({:10}): {:6.0}ms | {:6.0} records/sec",
        encoding_name,
        "SERDE",
        duration.as_millis(),
        rate
    );
}

fn test_serde_serialization_batched(indexed_comments: &HashMap<String, CommentData>, encoding: Encoding) {
    let encoding_name = encoding.to_string();
    println!(
        "\nTesting {} batched serialization (100 records per batch):",
        encoding_name
    );

    let start = Instant::now();

    // Split data into batches of 100
    let batch_size = 100;
    let comments: Vec<_> = indexed_comments.iter().collect();
    let total_batches = (comments.len() + batch_size - 1) / batch_size;

    for (batch_num, chunk) in comments.chunks(batch_size).enumerate() {
        // Create a HashMap for this batch
        let mut batch_data = HashMap::new();
        for &(key, value) in chunk {
            batch_data.insert(key.clone(), value.clone());
        }

        // Get the file suffix
        let suffix = match encoding {
            Encoding::JSON => "json",
            Encoding::POSTCARD => "postcard",
            Encoding::MESSAGEPACK => "msgpack",
            Encoding::BINCODE => "bincode",
            Encoding::CBOR => "cbor",
            Encoding::PROST => "prost",
        };

        // Serialize this batch to a separate file
        let path = format!(
            "tmp/batch_{}_{}_batch_{}.{}",
            encoding.to_string(),
            batch_num,
            batch_size,
            suffix
        );

        index::serde::serialize_and_write_to_path(encoding.clone(), &batch_data, &path, true);
    }

    let duration = start.elapsed();
    let rate = SCALE as f64 / duration.as_secs_f64();

    println!(
        "{} batched serialization ({:10}): {:6.0}ms | {:6.0} records/sec | {} batches",
        encoding_name,
        "SERDE",
        duration.as_millis(),
        rate,
        total_batches
    );
}

fn setup_table(conn: &Connection) -> Result<(), Box<dyn std::error::Error>> {
    conn.execute(
        "CREATE TABLE IF NOT EXISTS comments (
            id INTEGER PRIMARY KEY,
            comment TEXT,
            entry_name TEXT,
            created_at INTEGER,
            score INTEGER
        )",
        [],
    )?;

    // Configure SQLite for maximum write performance
    conn.pragma_update(None, "journal_mode", "WAL")?;
    conn.pragma_update(None, "synchronous", "NORMAL")?;

    Ok(())
}

fn create_indexes(conn: &Connection) -> Result<(), Box<dyn std::error::Error>> {
    println!("  🔨 Creating entry_name index before insertion...");
    conn.execute("CREATE INDEX idx_entry_name ON comments(entry_name)", [])?;
    Ok(())
}

fn test_sql_insertion_batch_100(
    conn: &Connection,
    indexed_comments: &HashMap<String, CommentData>,
    scenario: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    conn.execute("DELETE FROM comments", [])?;
    let start = Instant::now();

    let batch_size = 100;
    let mut records_inserted = 0;
    let comments: Vec<_> = indexed_comments.values().collect();

    while records_inserted < SCALE {
        let tx = conn.unchecked_transaction()?;
        let mut stmt =
            tx.prepare("INSERT INTO comments (comment, entry_name, created_at, score) VALUES (?1, ?2, ?3, ?4)")?;

        let batch_end = std::cmp::min(records_inserted + batch_size, SCALE);
        for i in records_inserted..batch_end {
            let comment_data = &comments[i];
            let created_at = 1600000000 + (i * 60) as i64;
            let score = (i % 100 + 1) as i64;

            stmt.execute([
                &comment_data.text,
                &comment_data.entry_name,
                &created_at.to_string(),
                &score.to_string(),
            ])?;
        }

        drop(stmt);
        tx.commit()?;
        records_inserted = batch_end;
    }

    let duration = start.elapsed();
    let rate = SCALE as f64 / duration.as_secs_f64();

    println!(
        "Transaction batch 100 ({:10}): {:6.0}ms | {:6.0} records/sec",
        scenario,
        duration.as_millis(),
        rate
    );

    Ok(())
}

fn test_sharded_sql_insertion(
    indexed_comments: &HashMap<String, CommentData>,
) -> Result<(), Box<dyn std::error::Error>> {
    let db_path = format!("tmp/sharded_write_benchmark_{}.db", SCALE);

    // Clean setup
    std::fs::remove_file(&db_path).ok();
    let mut conn = Connection::open(&db_path)?;

    // Configure SQLite for maximum write performance
    conn.pragma_update(None, "journal_mode", "WAL")?;
    conn.pragma_update(None, "synchronous", "NORMAL")?;

    let start = std::time::Instant::now();

    // Calculate batch size - aim for ~100 records per table (same as storage benchmark)
    let batch_size = 100;

    let tx = conn.transaction()?;

    let mut batch_id = 0;
    let mut current_batch_count = 0;
    let mut current_table_name = format!("comments_batch_{}", batch_id);

    // Create first table
    tx.execute(
        &format!(
            "CREATE TABLE {} (
                id INTEGER PRIMARY KEY,
                comment TEXT,
                entry_name TEXT,
                created_at INTEGER,
                score INTEGER
            )",
            current_table_name
        ),
        [],
    )?;

    let mut stmt = tx.prepare(&format!(
        "INSERT INTO {} (comment, entry_name, created_at, score) VALUES (?1, ?2, ?3, ?4)",
        current_table_name
    ))?;

    for (i, comment_data) in indexed_comments.values().enumerate() {
        // Check if we need to create a new table for this batch
        if current_batch_count >= batch_size {
            // Finalize current statement
            drop(stmt);

            // Create new table for next batch
            batch_id += 1;
            current_table_name = format!("comments_batch_{}", batch_id);
            current_batch_count = 0;

            tx.execute(
                &format!(
                    "CREATE TABLE {} (
                        id INTEGER PRIMARY KEY,
                        comment TEXT,
                        entry_name TEXT,
                        created_at INTEGER,
                        score INTEGER
                    )",
                    current_table_name
                ),
                [],
            )?;

            // Create new prepared statement for new table
            stmt = tx.prepare(&format!(
                "INSERT INTO {} (comment, entry_name, created_at, score) VALUES (?1, ?2, ?3, ?4)",
                current_table_name
            ))?;
        }

        let created_at = 1600000000 + (i * 60) as i64;
        let score = (i % 100 + 1) as i64;

        stmt.execute([
            &comment_data.text,
            &comment_data.entry_name,
            &created_at.to_string(),
            &score.to_string(),
        ])?;

        current_batch_count += 1;
    }

    drop(stmt);
    tx.commit()?;

    let duration = start.elapsed();
    let rate = SCALE as f64 / duration.as_secs_f64();

    println!(
        "Sharded tables       ({:10}): {:6.0}ms | {:6.0} records/sec | {} tables",
        "sharded",
        duration.as_millis(),
        rate,
        batch_id + 1
    );

    Ok(())
}
