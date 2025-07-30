use rand::prelude::*;
use rand::seq::SliceRandom;
use rusqlite::{Connection, Result};
use std::time::Instant;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("SQLite Read Performance Benchmark");
    println!("==================================");

    // Test different scales
    let test_scales = vec![1_000, 10_000, 100_000, 1_000_000];

    for &scale in &test_scales {
        println!("\nTesting {} records:", scale);
        println!("----------------------------------------");

        // Setup database with data for this scale
        let db_path = format!("tmp/read_benchmark_{}.db", scale);
        let conn = Connection::open(&db_path)?;
        setup_and_populate_table(&conn, scale)?;

        // Test the 3 specific scenarios
        test_entry_name_search(&conn, scale)?;
        test_select_all_rows(&conn, scale)?;
        test_select_rows_one_by_one(&conn, scale)?;
    }

    Ok(())
}

fn setup_and_populate_table(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
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

    // Populate with test data if not already populated
    let count: i64 = conn.query_row("SELECT COUNT(*) FROM comments", [], |row| row.get(0))?;
    if count == 0 {
        println!("  Populating database with {} records...", scale);

        // Use efficient batch insertion
        let tx = conn.unchecked_transaction()?;
        let batch_size = 400;
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
    }

    Ok(())
}

fn test_select_rows_one_by_one(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
    // First, get all IDs from the database
    let mut all_ids = Vec::new();
    let mut stmt = conn.prepare("SELECT id FROM comments")?;
    let id_rows = stmt.query_map([], |row| row.get::<_, i64>(0))?;

    for id_result in id_rows {
        all_ids.push(id_result?);
    }

    // Shuffle the IDs for random access pattern
    let mut rng = rand::thread_rng();
    all_ids.shuffle(&mut rng);

    // Test all IDs
    let num_queries = all_ids.len();
    let test_ids = &all_ids;

    let start = Instant::now();

    for &id in test_ids {
        let (_comment, _entry_name): (String, String) =
            conn.query_row("SELECT comment, entry_name FROM comments WHERE id = ?1", [id], |row| {
                Ok((row.get(0)?, row.get(1)?))
            })?;
    }

    let duration = start.elapsed();
    let rate = num_queries as f64 / duration.as_secs_f64();

    println!(
        "Select rows 1 by 1    : {:.0}ms  |  {:.0} queries/sec  |  {} total queries",
        duration.as_millis(),
        rate,
        num_queries
    );

    Ok(())
}

fn test_batch_selects(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
    let batch_sizes = vec![10, 50, 100];
    let num_queries = std::cmp::min(1000, scale);
    let mut rng = rand::thread_rng();

    for &batch_size in &batch_sizes {
        let start = Instant::now();
        let mut queries_executed = 0;

        while queries_executed < num_queries {
            let mut stmt = conn.prepare("SELECT comment FROM comments WHERE id = ?1")?;

            let batch_end = std::cmp::min(queries_executed + batch_size, num_queries);
            for _ in queries_executed..batch_end {
                let random_id = rng.gen_range(1..=scale);
                let _: String = stmt.query_row([random_id], |row| row.get(0))?;
            }

            queries_executed = batch_end;
        }

        let duration = start.elapsed();
        let rate = num_queries as f64 / duration.as_secs_f64();

        println!(
            "Batch selects   {:3}   : {:.0}ms  |  {:.0} queries/sec",
            batch_size,
            duration.as_millis(),
            rate
        );
    }

    Ok(())
}

fn test_range_queries(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();
    let num_queries = 100; // Range queries return multiple rows, so fewer iterations
    let mut rng = rand::thread_rng();
    let mut total_rows = 0;

    for _ in 0..num_queries {
        let start_id = rng.gen_range(1..=scale.saturating_sub(100));
        let end_id = start_id + 100; // Get 100 records each time

        let mut stmt = conn.prepare("SELECT comment FROM comments WHERE id BETWEEN ?1 AND ?2")?;
        let rows = stmt.query_map([start_id, end_id], |row| row.get::<_, String>(0))?;

        for row in rows {
            let _comment = row?;
            total_rows += 1;
        }
    }

    let duration = start.elapsed();
    let rate = total_rows as f64 / duration.as_secs_f64();

    println!(
        "Range queries         : {:.0}ms  |  {:.0} rows/sec  |  {} total rows",
        duration.as_millis(),
        rate,
        total_rows
    );

    Ok(())
}

fn test_text_searches(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();
    let num_queries = 50; // Text searches can be slower
    let mut rng = rand::thread_rng();
    let mut total_rows = 0;

    for _ in 0..num_queries {
        let random_num = rng.gen_range(0..scale);
        let search_pattern = format!("comment_{:06}%", random_num);

        let mut stmt = conn.prepare("SELECT comment FROM comments WHERE comment LIKE ?1")?;
        let rows = stmt.query_map([search_pattern], |row| row.get::<_, String>(0))?;

        for row in rows {
            let _comment = row?;
            total_rows += 1;
        }
    }

    let duration = start.elapsed();
    let rate = total_rows as f64 / duration.as_secs_f64();

    println!(
        "Text searches (LIKE)  : {:.0}ms  |  {:.0} rows/sec  |  {} total rows",
        duration.as_millis(),
        rate,
        total_rows
    );

    Ok(())
}

fn test_with_pragmas(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
    // Set read performance pragmas
    conn.pragma_update(None, "cache_size", 10000)?;
    conn.pragma_update(None, "temp_store", "memory")?;
    conn.pragma_update(None, "mmap_size", 268435456)?; // 256MB

    let start = Instant::now();
    let num_queries = std::cmp::min(1000, scale);
    let mut rng = rand::thread_rng();

    for _ in 0..num_queries {
        let random_id = rng.gen_range(1..=scale);
        let _: String = conn.query_row("SELECT comment FROM comments WHERE id = ?1", [random_id], |row| {
            row.get(0)
        })?;
    }

    let duration = start.elapsed();
    let rate = num_queries as f64 / duration.as_secs_f64();

    println!(
        "With read pragmas     : {:.0}ms  |  {:.0} queries/sec",
        duration.as_millis(),
        rate
    );

    Ok(())
}

fn test_entry_name_search(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
    // Create index on entry_name for testing
    conn.execute("CREATE INDEX IF NOT EXISTS idx_entry_name ON comments(entry_name)", [])?;

    // Get all unique entry names from the database
    let mut all_entry_names = Vec::new();
    let mut stmt = conn.prepare("SELECT DISTINCT entry_name FROM comments")?;
    let entry_rows = stmt.query_map([], |row| row.get::<_, String>(0))?;

    for entry_result in entry_rows {
        all_entry_names.push(entry_result?);
    }

    let start = Instant::now();
    let num_queries = all_entry_names.len();
    let mut total_rows = 0;

    for entry_name in &all_entry_names {
        let mut stmt = conn.prepare("SELECT id, comment FROM comments WHERE entry_name = ?1")?;
        let rows = stmt.query_map([entry_name], |row| {
            Ok((row.get::<_, i64>(0)?, row.get::<_, String>(1)?))
        })?;

        for row in rows {
            let (_id, _comment) = row?;
            total_rows += 1;
        }
    }

    let duration = start.elapsed();
    let searches_per_sec = num_queries as f64 / duration.as_secs_f64();
    let rows_per_sec = total_rows as f64 / duration.as_secs_f64();

    println!(
        "Entry name search     : {:.0}ms  |  {:.0} searches/sec  |  {:.0} rows/sec  |  {} total rows",
        duration.as_millis(),
        searches_per_sec,
        rows_per_sec,
        total_rows
    );

    Ok(())
}

fn test_select_all_rows(conn: &Connection, scale: usize) -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();
    let mut total_rows = 0;

    let mut stmt = conn.prepare("SELECT id, comment, entry_name FROM comments")?;
    let rows = stmt.query_map([], |row| {
        Ok((
            row.get::<_, i64>(0)?,
            row.get::<_, String>(1)?,
            row.get::<_, String>(2)?,
        ))
    })?;

    for row in rows {
        let (_id, _comment, _entry_name) = row?;
        total_rows += 1;
    }

    let duration = start.elapsed();
    let rows_per_sec = total_rows as f64 / duration.as_secs_f64();

    println!(
        "Select all rows       : {:.0}ms  |  {:.0} rows/sec  |  {} total rows",
        duration.as_millis(),
        rows_per_sec,
        total_rows
    );

    Ok(())
}
