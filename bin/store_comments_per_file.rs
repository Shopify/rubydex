use index::comments::CommentData;
use rusqlite::{Connection, Result, params};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let conn: Connection = Connection::open("tmp/comments.db")?;

    conn.execute("DELETE FROM comments", [])?;
    conn.execute("VACUUM", [])?;

    // Generate comments for 1 different files.
    for i in 0..100000 {
        println!("{}", i);
        // Generate 100 comments per file
        for j in 0..100 {
            let entry_name = format!("entry_{i}_{j}");
            let comment: CommentData = CommentData::new(entry_name, 100);
            conn.execute(
                "INSERT INTO comments (comment, entry_name) VALUES (?1, ?2)",
                [comment.text, comment.entry_name],
            )?;
        }
    }
    println!("insert successful!");
    Ok(())
}
