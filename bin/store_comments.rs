use index::comments::CommentData;
use rusqlite::{Connection, Result, params};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let conn: Connection = Connection::open("tmp/comments.db")?;

    conn.execute("DELETE FROM comments", [])?;
    conn.execute("VACUUM", [])?;

    // Generate 10 different comments of 1000 chars each.
    for i in 0..10000 {
        let entry_name = format!("entry_{i}");
        let comment: CommentData = CommentData::new(entry_name, 1);
        conn.execute(
            "INSERT INTO comments (comment, entry_name) VALUES (?1, ?2)",
            [comment.text, comment.entry_name],
        )?;
        println!("insert successful!");

        // Now we test retrieving the comment
        let mut stmt = conn.prepare("SELECT comment, entry_name FROM comments")?;
        let comments = stmt.query_map([], |row| {
            Ok(CommentData {
                text: row.get(0)?,
                entry_name: row.get(1)?,
            })
        })?;
    }

    Ok(())
}
