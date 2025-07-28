use index::comments::CommentData;
use index::serde::Encoding;
use rand::prelude::*;
use rand::seq::SliceRandom;
use rusqlite::{Connection, Result};
use std::{collections::HashMap, time::Instant};

// NOTE: Change these values determine size of DB
// and number of lookups to perform
static SCALE: usize = 1_000_000;
static NUM_LOOKUPS: usize = 1000;

static ENCODINGS_LIST: [Encoding; 6] = [
    Encoding::JSON,
    Encoding::POSTCARD,
    Encoding::MESSAGEPACK,
    Encoding::BINCODE,
    Encoding::CBOR,
    Encoding::PROST,
];

// The purpose of this Benchmark is to test how quickly each storage
// method can look up a given number of records from a database.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut indexed_comments = HashMap::new();
    for i in 0..SCALE {
        let entry_name = format!("entry_{}", i);
        let comment = CommentData::new(entry_name.clone(), 100);
        indexed_comments.insert(entry_name, comment);
    }
    println!("Created {} comments in memory", SCALE);

    // Setup SQLite database with data
    let db_path = format!("tmp/read_benchmark_{}.db", SCALE);
    let mut conn = Connection::open(&db_path)?;
    setup_and_populate_table(&mut conn, &indexed_comments)?;
    println!("  📝 Populated SQLite database with {} records...", SCALE);

    // Setup serialized files with data
    for encoding in ENCODINGS_LIST.clone() {
        let encoding_name = encoding.to_string();
        index::serde::serialize_and_write(encoding, &indexed_comments);
        println!("  📝 Wrote to file buffer as {}", encoding_name);
    }

    // Get random sample of entry_names for testing
    let test_entry_names = get_random_entry_names(&conn)?;
    println!("📋 Prepared {} random entry_names for testing", test_entry_names.len());

    // Test WITHOUT entry_name index first
    println!("\n🚫 WITHOUT entry_name INDEX:");
    println!("-----------------------------");
    ensure_no_index(&conn)?;
    test_entry_name_lookups_sql(&conn, &test_entry_names, "NO INDEX")?;

    // Load all SQLite records into memory for in-memory lookup testing
    println!("\n💾 IN-MEMORY SQLite query result lookups:");
    println!("-----------------------------");
    ensure_no_index(&conn)?;
    test_entry_name_lookups_sqlite_memory(&conn, &test_entry_names)?;

    // Load all SQLite records into memory for linear search testing
    println!("\n🔍 IN-MEMORY SQLite linear search lookups:");
    println!("-----------------------------");
    test_entry_name_lookups_sqlite_linear_search(&conn, &test_entry_names)?;

    // Test WITH entry_name index
    println!("\n✅ WITH entry_name INDEX:");
    println!("--------------------------");
    create_index(&conn)?;
    test_entry_name_lookups_sql(&conn, &test_entry_names, "WITH INDEX")?;

    // Test reads for each serde encoding now
    for encoding in ENCODINGS_LIST.clone() {
        test_entry_name_lookups_serde(encoding, &test_entry_names);
    }

    Ok(())
}

fn setup_and_populate_table(
    conn: &mut Connection,
    indexed_comments: &HashMap<String, CommentData>,
) -> Result<(), Box<dyn std::error::Error>> {
    // Create table
    conn.execute("DROP TABLE IF EXISTS comments", [])?;
    conn.execute(
        "CREATE TABLE comments (
            id INTEGER PRIMARY KEY,
            comment TEXT,
            entry_name TEXT
        )",
        [],
    )?;
    conn.pragma_update(None, "journal_mode", "WAL")?;
    conn.pragma_update(None, "synchronous", "NORMAL")?;

    println!("  📝 Populating database with 1M records...");
    let comments = indexed_comments.values();

    // Use efficient batch insertion
    let batch_size = 400;
    let mut records_inserted = 0;
    let mut rng = rand::thread_rng();

    let tx = conn.transaction()?;
    {
        let mut stmt = tx.prepare("INSERT INTO comments (entry_name, comment) VALUES (?, ?)")?;

        for comment_data in comments {
            stmt.execute((comment_data.entry_name.clone(), comment_data.text.clone()))?;
        }
    }

    tx.commit()?;

    Ok(())
}

fn ensure_no_index(conn: &Connection) -> Result<(), Box<dyn std::error::Error>> {
    // Drop entry_name index (keep primary key)
    conn.execute("DROP INDEX IF EXISTS idx_entry_name", [])?;
    Ok(())
}

fn create_index(conn: &Connection) -> Result<(), Box<dyn std::error::Error>> {
    println!("  🔨 Creating entry_name index...");

    // Only create index on entry_name for lookups
    conn.execute("CREATE INDEX idx_entry_name ON comments(entry_name)", [])?;

    Ok(())
}

fn get_random_entry_names(conn: &Connection) -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let mut entry_names = Vec::new();
    let mut stmt = conn.prepare("SELECT DISTINCT entry_name FROM comments")?;
    let entry_rows = stmt.query_map([], |row| row.get::<_, String>(0))?;

    for entry_result in entry_rows {
        entry_names.push(entry_result?);
    }

    // Shuffle and take random sample
    let mut rng = rand::thread_rng();
    entry_names.shuffle(&mut rng);
    entry_names.truncate(NUM_LOOKUPS);

    Ok(entry_names)
}

fn test_entry_name_lookups_sql(
    conn: &Connection,
    test_entry_names: &[String],
    scenario: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();
    let mut total_rows = 0;

    for entry_name in test_entry_names {
        let mut stmt = conn.prepare("SELECT id, comment FROM comments WHERE entry_name = ?1")?;
        let rows = stmt.query_map([entry_name], |row| {
            Ok((row.get::<_, i64>(0)?, row.get::<_, String>(1)?))
        })?;

        for row in rows {
            let (id, comment) = row?;
        }
        total_rows += 1;
    }

    let duration = start.elapsed();
    let avg_per_lookup = duration.as_micros() as f64 / test_entry_names.len() as f64;
    let lookups_per_sec = test_entry_names.len() as f64 / duration.as_secs_f64();

    println!(
        "{} SQL lookups       ({:10}): {:6.0}ms total | {:6.1}μs avg/lookup | {:6.0} lookups/sec | {} rows found",
        NUM_LOOKUPS,
        scenario,
        duration.as_millis(),
        avg_per_lookup,
        lookups_per_sec,
        total_rows
    );

    Ok(())
}

fn load_all_records_to_memory(conn: &Connection) -> Result<HashMap<String, CommentData>, Box<dyn std::error::Error>> {
    let mut records_map = HashMap::new();
    let mut stmt = conn.prepare("SELECT entry_name, comment FROM comments")?;
    let rows = stmt.query_map([], |row| Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?)))?;

    for row_result in rows {
        let (entry_name, comment_text) = row_result?;
        let comment_data = CommentData {
            entry_name: entry_name.clone(),
            text: comment_text,
        };
        records_map.insert(entry_name, comment_data);
    }

    Ok(records_map)
}

fn test_entry_name_lookups_sqlite_memory(
    conn: &Connection,
    test_entry_names: &[String],
) -> Result<(), Box<dyn std::error::Error>> {
    let load_start = Instant::now();
    let records_map = load_all_records_to_memory(conn)?;
    let load_duration = load_start.elapsed();

    let lookup_start = Instant::now();
    let mut total_rows = 0;

    for entry_name in test_entry_names {
        if let Some(_comment_data) = records_map.get(entry_name) {
            total_rows += 1;
        }
    }

    let lookup_duration = lookup_start.elapsed();
    let total_duration = load_duration + lookup_duration;
    let avg_per_lookup = lookup_duration.as_micros() as f64 / test_entry_names.len() as f64;
    let lookups_per_sec = test_entry_names.len() as f64 / lookup_duration.as_secs_f64();

    println!(
        "{} SQLite memory lookups {:6.0}ms total | Load: {:6.0}ms | Lookup: {:6.0}ms | {:6.1}μs avg/lookup | {:6.0} lookups/sec | {} rows found",
        NUM_LOOKUPS,
        total_duration.as_millis(),
        load_duration.as_millis(),
        lookup_duration.as_millis(),
        avg_per_lookup,
        lookups_per_sec,
        total_rows
    );

    Ok(())
}

fn test_entry_name_lookups_sqlite_linear_search(
    conn: &Connection,
    test_entry_names: &[String],
) -> Result<(), Box<dyn std::error::Error>> {
    let load_start = Instant::now();

    // Query once and collect all rows into a Vec
    let mut stmt = conn.prepare("SELECT entry_name, comment FROM comments")?;
    let rows = stmt.query_map([], |row| Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?)))?;

    let all_rows: Result<Vec<_>, _> = rows.collect();
    let all_rows = all_rows?;

    let load_duration = load_start.elapsed();

    // Now perform lookups using linear search through the Vec
    let lookup_start = Instant::now();
    let mut total_rows = 0;
    for entry_name in test_entry_names {
        for (record_entry_name, _comment_text) in &all_rows {
            if record_entry_name == entry_name {
                total_rows += 1;
                break;
            }
        }
    }

    let lookup_duration = lookup_start.elapsed();
    let total_duration = load_duration + lookup_duration;
    let avg_per_lookup = lookup_duration.as_micros() as f64 / test_entry_names.len() as f64;
    let lookups_per_sec = test_entry_names.len() as f64 / lookup_duration.as_secs_f64();

    println!(
        "{} SQLite linear search  {:6.0}ms total | Load: {:6.0}ms | Lookup: {:6.0}ms | {:6.1}μs avg/lookup | {:6.0} lookups/sec | {} rows found",
        NUM_LOOKUPS,
        total_duration.as_millis(),
        load_duration.as_millis(),
        lookup_duration.as_millis(),
        avg_per_lookup,
        lookups_per_sec,
        total_rows
    );

    Ok(())
}

fn test_entry_name_lookups_serde(encoding: Encoding, test_entry_names: &[String]) {
    let _encoding_name = encoding.to_string();
    let load_start = Instant::now();
    let deserialized_comments_hash = index::serde::deserialize_from_file(encoding.clone()).unwrap();
    let load_duration = load_start.elapsed();

    let lookup_start = Instant::now();
    let mut total_rows = 0;

    for entry_name in test_entry_names {
        let comment = &deserialized_comments_hash[entry_name];
        total_rows += 1;
    }

    let lookup_duration = lookup_start.elapsed();
    let total_duration = load_duration + lookup_duration;
    let avg_per_lookup = lookup_duration.as_micros() as f64 / test_entry_names.len() as f64;
    let lookups_per_sec = test_entry_names.len() as f64 / lookup_duration.as_secs_f64();
    println!(
        "{} {} lookups       {:6.0}ms total | Load: {:6.0}ms | Lookup: {:6.0}ms | {:6.1}μs avg/lookup | {:6.0} lookups/sec | {} rows found",
        NUM_LOOKUPS,
        encoding.to_string(),
        total_duration.as_millis(),
        load_duration.as_millis(),
        lookup_duration.as_millis(),
        avg_per_lookup,
        lookups_per_sec,
        total_rows
    );
}
