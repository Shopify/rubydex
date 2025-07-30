use rusqlite::{Connection, Result};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let conn = Connection::open("tmp/entries.db")?;
    // Now let's try to create a table to store comments

    conn.execute(
        "create table if not exists entries (
             id integer primary key,
             name text not null,
             value text not null
         )",
        (),
    )?;

    conn.execute("create index if not exists idx_comments_entry_name on name (name)", ())?;
    Ok(())
}
