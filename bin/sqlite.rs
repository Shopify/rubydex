use rusqlite::{Connection, Result};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let conn = Connection::open("tmp/comments.db")?;
    // Now let's try to create a table to store comments

    conn.execute(
        "create table if not exists comments (
             id integer primary key,
             comment text not null,
             entry_name text not null
         )",
        (),
    )?;

    conn.execute(
        "create index if not exists idx_comments_entry_name on comments (entry_name)",
        (),
    )?;
    Ok(())
}
