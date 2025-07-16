use rusqlite::{Connection, Result};
use std::time::Instant;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("SQLite Write Performance Benchmark");
    println!("===================================");

    // Test different scales
    let test_scales = vec![1_000, 10_000, 100_000, 1_000_000];

    for &scale in &test_scales {
        println!("\nTesting {} records:", scale);
        println!("----------------------------------------");

        // Clean setup for each scale
        let db_path = format!("tmp/benchmark_{}.db", scale);
        std::fs::remove_file(&db_path).ok();
        let conn = Connection::open(&db_path)?;
        setup_table(&conn)?;

        // Test different approaches
        if scale <= 100_000 {
            test_individual_transactions(&conn, scale)?;
        }
        test_transaction_batches(&conn, scale)?;
        test_with_pragmas(&conn, scale)?;
        test_batch_inserts(&conn, scale)?;
    }

    Ok(())
}

fn setup_table(conn: &Connection) -> Result<(), Box<dyn std::error::Error>> {
    conn.execute(
        "CREATE TABLE IF NOT EXISTS comments (
            id INTEGER PRIMARY KEY,
            comment TEXT,
            entry_name TEXT
        )",
        [],
    )?;
    Ok(())
}

fn test_individual_transactions(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
    conn.execute("DELETE FROM comments", [])?;
    let start = Instant::now();

    for i in 0..scale {
        let comment = format!("comment_{:06}", i);
        let entry = format!("entry_{}", i / 100);

        conn.execute(
            "INSERT INTO comments (comment, entry_name) VALUES (?1, ?2)",
            [&comment, &entry],
        )?;
    }

    let duration = start.elapsed();
    let rate = scale as f64 / duration.as_secs_f64();

    println!(
        "Individual transactions: {:.0}ms  |  {:.0} records/sec",
        duration.as_millis(),
        rate
    );

    Ok(())
}

fn test_transaction_batches(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
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
            let mut stmt = tx.prepare("INSERT INTO comments (comment, entry_name) VALUES (?1, ?2)")?;

            let batch_end = std::cmp::min(records_inserted + batch_size, scale);
            for i in records_inserted..batch_end {
                let comment = format!("comment_{:06}", i);
                let entry = format!("entry_{}", i / 100);
                stmt.execute([&comment, &entry])?;
            }

            drop(stmt);
            tx.commit()?;
            records_inserted = batch_end;
        }

        let duration = start.elapsed();
        let rate = scale as f64 / duration.as_secs_f64();

        println!(
            "Transaction batch {:4}: {:.0}ms  |  {:.0} records/sec",
            batch_size,
            duration.as_millis(),
            rate
        );
    }

    Ok(())
}

fn test_with_pragmas(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
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
        let mut stmt = tx.prepare("INSERT INTO comments (comment, entry_name) VALUES (?1, ?2)")?;

        let batch_end = std::cmp::min(records_inserted + batch_size, scale);
        for i in records_inserted..batch_end {
            let comment = format!("comment_{:06}", i);
            let entry = format!("entry_{}", i / 100);
            stmt.execute([&comment, &entry])?;
        }

        drop(stmt);
        tx.commit()?;
        records_inserted = batch_end;
    }

    let duration = start.elapsed();
    let rate = scale as f64 / duration.as_secs_f64();

    println!(
        "With pragmas + batch  : {:.0}ms  |  {:.0} records/sec",
        duration.as_millis(),
        rate
    );

    // Reset pragmas
    conn.pragma_update(None, "journal_mode", "DELETE")?;
    conn.pragma_update(None, "synchronous", "FULL")?;

    Ok(())
}

fn test_batch_inserts(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
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
            values.push("(?, ?)");
            params.push(format!("comment_{:06}", i));
            params.push(format!("entry_{}", i / 100));
        }

        let sql = format!("INSERT INTO comments (comment, entry_name) VALUES {}", values.join(","));
        tx.execute(&sql, rusqlite::params_from_iter(&params))?;

        records_inserted = batch_end;
    }

    tx.commit()?;

    let duration = start.elapsed();
    let rate = scale as f64 / duration.as_secs_f64();

    println!(
        "Batch SQL inserts     : {:.0}ms  |  {:.0} records/sec",
        duration.as_millis(),
        rate
    );

    Ok(())
}
