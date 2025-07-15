use index::comments::CommentData;
use rusqlite::{Connection, Result, params};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let comment = CommentData::new(String::from("entry_1"), 100);
    let conn = Connection::open("tmp/comments.db")?;
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

    println!("comments: {:?}", comments.collect::<Vec<_>>());

    Ok(())
}
