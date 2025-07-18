use rusqlite::{Connection, Result};
use std::time::Instant;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("SQLite Write Performance Benchmark: 1M Records with Batch Size 100");
    println!("===================================================================");
    println!("Comparing WITH vs WITHOUT entry_name index");

    let scale = 1_000_000;

    // Test WITHOUT entry_name index first (faster inserts)
    println!("\n🚫 WITHOUT entry_name INDEX:");
    println!("-----------------------------");
    test_insertion_scenario(&scale, false)?;

    // Test WITH entry_name index (slower inserts due to index maintenance)
    println!("\n✅ WITH entry_name INDEX:");
    println!("--------------------------");
    test_insertion_scenario(&scale, true)?;

    Ok(())
}

fn test_insertion_scenario(scale: &usize, with_indexes: bool) -> Result<(), Box<dyn std::error::Error>> {
    let scenario_name = if with_indexes { "indexed" } else { "no_indexes" };
    let db_path = format!("tmp/write_benchmark_{}_{}.db", scenario_name, scale);

    // Clean setup for each test
    std::fs::remove_file(&db_path).ok();
    let conn = Connection::open(&db_path)?;
    setup_table(&conn)?;

    if with_indexes {
        create_indexes(&conn)?;
    }

    // Test transaction batches with size 100
    test_transaction_batch_100(&conn, *scale, scenario_name)?;

    if with_indexes {
        println!("  📊 Index overhead comparison:");
        measure_index_overhead(&conn, *scale)?;
    }

    Ok(())
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
    Ok(())
}

fn create_indexes(conn: &Connection) -> Result<(), Box<dyn std::error::Error>> {
    println!("  🔨 Creating entry_name index before insertion...");

    // Only create index on entry_name for lookups
    conn.execute("CREATE INDEX idx_entry_name ON comments(entry_name)", [])?;

    Ok(())
}

fn test_individual_transactions(
    conn: &Connection,
    scale: usize,
    scenario: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    conn.execute("DELETE FROM comments", [])?;
    let start = Instant::now();

    for i in 0..scale {
        let comment = format!("comment_{:06}", i);
        let entry = format!("entry_{}", i / 100);
        let created_at = 1600000000 + (i * 60) as i64;
        let score = (i % 100 + 1) as i64;

        conn.execute(
            "INSERT INTO comments (comment, entry_name, created_at, score) VALUES (?1, ?2, ?3, ?4)",
            [&comment, &entry, &created_at.to_string(), &score.to_string()],
        )?;
    }

    let duration = start.elapsed();
    let rate = scale as f64 / duration.as_secs_f64();

    println!(
        "Individual transactions ({:10}): {:6.0}ms | {:6.0} records/sec",
        scenario,
        duration.as_millis(),
        rate
    );

    Ok(())
}

fn test_transaction_batches(conn: &Connection, scale: usize, scenario: &str) -> Result<(), Box<dyn std::error::Error>> {
    let batch_sizes = vec![10, 100, 1000];

    for &batch_size in &batch_sizes {
        if batch_size > scale {
            continue;
        }

        conn.execute("DELETE FROM comments", [])?;
        let start = Instant::now();

        let mut records_inserted = 0;
        while records_inserted < scale {
            let tx = conn.unchecked_transaction()?;
            let mut stmt =
                tx.prepare("INSERT INTO comments (comment, entry_name, created_at, score) VALUES (?1, ?2, ?3, ?4)")?;

            let batch_end = std::cmp::min(records_inserted + batch_size, scale);
            for i in records_inserted..batch_end {
                let comment = format!("comment_{:06}", i);
                let entry = format!("entry_{}", i / 100);
                let created_at = 1600000000 + (i * 60) as i64;
                let score = (i % 100 + 1) as i64;
                stmt.execute([&comment, &entry, &created_at.to_string(), &score.to_string()])?;
            }

            drop(stmt);
            tx.commit()?;
            records_inserted = batch_end;
        }

        let duration = start.elapsed();
        let rate = scale as f64 / duration.as_secs_f64();

        println!(
            "Transaction batch {:4} ({:10}): {:6.0}ms | {:6.0} records/sec",
            batch_size,
            scenario,
            duration.as_millis(),
            rate
        );
    }

    Ok(())
}

fn test_transaction_batch_100(
    conn: &Connection,
    scale: usize,
    scenario: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    conn.execute("DELETE FROM comments", [])?;
    let start = Instant::now();

    let batch_size = 100;
    let mut records_inserted = 0;

    while records_inserted < scale {
        let tx = conn.unchecked_transaction()?;
        let mut stmt =
            tx.prepare("INSERT INTO comments (comment, entry_name, created_at, score) VALUES (?1, ?2, ?3, ?4)")?;

        let batch_end = std::cmp::min(records_inserted + batch_size, scale);
        for i in records_inserted..batch_end {
            let comment = format!("comment_{:06}", i);
            let entry = format!("entry_{}", i / 100);
            let created_at = 1600000000 + (i * 60) as i64;
            let score = (i % 100 + 1) as i64;
            stmt.execute([&comment, &entry, &created_at.to_string(), &score.to_string()])?;
        }

        drop(stmt);
        tx.commit()?;
        records_inserted = batch_end;
    }

    let duration = start.elapsed();
    let rate = scale as f64 / duration.as_secs_f64();

    println!(
        "Transaction batch 100 ({:10}): {:6.0}ms | {:6.0} records/sec",
        scenario,
        duration.as_millis(),
        rate
    );

    Ok(())
}

fn test_with_pragmas(conn: &Connection, scale: usize, scenario: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Set performance pragmas
    conn.pragma_update(None, "journal_mode", "WAL")?;
    conn.pragma_update(None, "synchronous", "NORMAL")?;
    conn.pragma_update(None, "cache_size", 10000)?;
    conn.pragma_update(None, "temp_store", "memory")?;

    conn.execute("DELETE FROM comments", [])?;
    let start = Instant::now();

    // Use 100-record batches (good middle ground)
    let batch_size = 100;
    let mut records_inserted = 0;

    while records_inserted < scale {
        let tx = conn.unchecked_transaction()?;
        let mut stmt =
            tx.prepare("INSERT INTO comments (comment, entry_name, created_at, score) VALUES (?1, ?2, ?3, ?4)")?;

        let batch_end = std::cmp::min(records_inserted + batch_size, scale);
        for i in records_inserted..batch_end {
            let comment = format!("comment_{:06}", i);
            let entry = format!("entry_{}", i / 100);
            let created_at = 1600000000 + (i * 60) as i64;
            let score = (i % 100 + 1) as i64;
            stmt.execute([&comment, &entry, &created_at.to_string(), &score.to_string()])?;
        }

        drop(stmt);
        tx.commit()?;
        records_inserted = batch_end;
    }

    let duration = start.elapsed();
    let rate = scale as f64 / duration.as_secs_f64();

    println!(
        "With pragmas + batch ({:10}): {:6.0}ms | {:6.0} records/sec",
        scenario,
        duration.as_millis(),
        rate
    );

    // Reset pragmas
    conn.pragma_update(None, "journal_mode", "DELETE")?;
    conn.pragma_update(None, "synchronous", "FULL")?;

    Ok(())
}

fn test_batch_inserts(conn: &Connection, scale: usize, scenario: &str) -> Result<(), Box<dyn std::error::Error>> {
    conn.execute("DELETE FROM comments", [])?;
    let start = Instant::now();

    let tx = conn.unchecked_transaction()?;
    let batch_size = 400; // Under SQLite's parameter limit

    let mut records_inserted = 0;
    while records_inserted < scale {
        let mut values = Vec::new();
        let mut params = Vec::new();

        let batch_end = std::cmp::min(records_inserted + batch_size, scale);
        for i in records_inserted..batch_end {
            values.push("(?, ?, ?, ?)");
            params.push(format!("comment_{:06}", i));
            params.push(format!("entry_{}", i / 100));
            params.push((1600000000 + i * 60).to_string());
            params.push(((i % 100) + 1).to_string());
        }

        let sql = format!(
            "INSERT INTO comments (comment, entry_name, created_at, score) VALUES {}",
            values.join(",")
        );
        tx.execute(&sql, rusqlite::params_from_iter(&params))?;

        records_inserted = batch_end;
    }

    tx.commit()?;

    let duration = start.elapsed();
    let rate = scale as f64 / duration.as_secs_f64();

    println!(
        "Batch SQL inserts    ({:10}): {:6.0}ms | {:6.0} records/sec",
        scenario,
        duration.as_millis(),
        rate
    );

    Ok(())
}

fn measure_index_overhead(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
    // Measure time to rebuild entry_name index on existing data
    println!("    Measuring entry_name index maintenance overhead...");

    // Drop entry_name index
    conn.execute("DROP INDEX IF EXISTS idx_entry_name", [])?;

    // Measure time to create entry_name index on populated table
    let start = Instant::now();

    conn.execute("CREATE INDEX idx_entry_name ON comments(entry_name)", [])?;

    let duration = start.elapsed();

    println!(
        "    Index creation time        : {:6.0}ms | {} records indexed",
        duration.as_millis(),
        scale
    );

    // Show storage overhead
    let table_size: i64 = conn.query_row(
        "SELECT page_count * page_size as size FROM pragma_page_count(), pragma_page_size()",
        [],
        |row| row.get(0),
    )?;

    println!("    Database size with indexes : {:6.0} KB", table_size as f64 / 1024.0);

    Ok(())
}
